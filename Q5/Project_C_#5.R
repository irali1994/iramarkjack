library('csv')
library('dplyr')
library('readxl')
library('aTSA')
library('outliers')
library('quantmod')
library('ggplot2')

#---------------------------------------------------------------------------------------#
#5 correlation
#---------------------------------------------------------------------------------------#
#import the data
setwd('~/2 ?????? Academics/Finance/15.458 & 459 Financial Data Science and Computing/Projects/Project C/Q5')
dt5_comp <- read.csv("Q5 query - Comp Stocks.csv",header=TRUE)
dt5_djia <- read.csv("Q5 query - DJIA.csv",header=TRUE)
dt5_comp = as.data.frame(dt5_comp)
dt5_comp$d = as.Date(dt5_comp$d,"%Y-%m-%d")
dt5_comp = dt5_comp[order(dt5_comp$d),]

#check integrity (505 days in total)
test_group = group_by(dt5_comp,ticker)
test_statistis = summarise(test_group,
                           count = n()
)

#---------------------------------------------------------------------------------------#
#clean the data to match djia composition
row_to_keep = which((dt5_comp$d < 2018-02-18 & !(dt5_comp$ticker %in% c('BAC','CVX','KFT','CSCO','TRV'))) | 
                      (dt5_comp$d > 2018-02-18 & dt5_comp$d < 2018-09-22 & !(dt5_comp$ticker %in% c('MO','HON','KFT','CSCO','TRV'))) |
                      (dt5_comp$d > 2018-09-22 & dt5_comp$d < 2019-06-08 & !(dt5_comp$ticker %in% c('MO','HON','AIG','CSCO','TRV'))) |
                      (dt5_comp$d > 2019-06-08 & !(dt5_comp$ticker %in% c('MO','HON','AIG','C','GM')))
)

dt5_comp = dt5_comp[row_to_keep,]

grouped_data <- group_by(dt5_comp,d)
statistics <- summarise(grouped_data,
                        count = n(),
                        total_p = sum(price)#,
                        #weight_sum = sum(weight)
) 
statistics = as.data.frame(statistics)

dt5_comp$weight = NA

#---------------------------------------------------------------------------------------#
#assign weight
for (i in 1:nrow(dt5_comp)) {
  for (j in 1:30) {
    dt5_comp$weight[i] = dt5_comp$price[i] / statistics$total_p[ceiling(i/30)]
  }
}

#---------------------------------------------------------------------------------------#
#calculate index variance (sigma_p^2)
DJIA_one_month = dt5_djia[dt5_djia$nm_numtype == 'vol_021',]
DJIA_three_month = dt5_djia[dt5_djia$nm_numtype == 'vol_063',]

DJIA_one_month$index_variance = DJIA_one_month$value^2
DJIA_three_month$index_variance = DJIA_three_month$value^2

#clean DJIA data
df_DJIA_1m = as.data.frame(DJIA_one_month)%>%
  select('d','index_variance')
df_DJIA_3m = as.data.frame(DJIA_three_month)%>%
  select('d','index_variance')
#---------------------------------------------------------------------------------------#
#prepare and calculate grouped data for results
#for one month window
group_1m <- group_by(dt5_comp,d)
statistics_1m <- summarise(group_1m,
                           count = n(),
                           sigma_zero = sum(vol_021^2 * weight^2)
) 
statistics_1m = data.frame(statistics_1m)

#for three month window
group_3m <- group_by(dt5_comp,d)
statistics_3m <- summarise(group_3m,
                           count = n(),
                           sigma_zero = sum(vol_063^2 * weight^2)
) 
statistics_3m = as.data.frame(statistics_3m)

#assign value of variance when correlation equals 0  (sigma_0^2)

df_DJIA_1m$sigma_zero = statistics_1m$sigma_zero
df_DJIA_3m$sigma_zero = statistics_3m$sigma_zero

#---------------------------------------------------------------------------------------#
#calculate variance when correlation equals 1  (sigma_1^2)
#deno is denominator in the formula (=sigma(1)^2 - sigma(0)^2)
df_DJIA_1m$deno = 0
df_DJIA_3m$deno = 0

for (i in (30*(0:504)+1)) {    #i is start point of each day
  for (j in (i:(i+28))) {        #j is start point of calculation of each day
    for (k in (j:(i+29))) {        #k is end point of calculation of each day
      df_DJIA_1m$deno[ceiling(i/30)] = df_DJIA_1m$deno[ceiling(i/30)] +
        2 * dt5_comp$vol_021[j] * dt5_comp$vol_021[k] * dt5_comp$weight[j] * dt5_comp$weight[k]
    }
  }
}

for (i in (30*(0:504)+1)) {    #i is start point of each day
  for (j in (i:(i+28))) {        #j is start point of calculation of each day
    for (k in (j:(i+29))) {        #k is end point of calculation of each day
      df_DJIA_3m$deno[ceiling(i/30)] = df_DJIA_3m$deno[ceiling(i/30)] +
        2 * dt5_comp$vol_063[j] * dt5_comp$vol_063[k] * dt5_comp$weight[j] * dt5_comp$weight[k]
    }
  }
}

df_DJIA_1m$sigma_one = df_DJIA_1m$sigma_zero + df_DJIA_1m$deno
df_DJIA_3m$sigma_one = df_DJIA_3m$sigma_zero + df_DJIA_3m$deno

#---------------------------------------------------------------------------------------#
#calculate average volatility
df_DJIA_1m$Rho = (df_DJIA_1m$index_variance - df_DJIA_1m$sigma_zero) / df_DJIA_1m$deno 
df_DJIA_3m$Rho = (df_DJIA_3m$index_variance - df_DJIA_3m$sigma_zero) / df_DJIA_3m$deno

#plot the result
df_DJIA_1m$d = as.Date(df_DJIA_1m$d,'%Y-%m-%d')
ggplot(df_DJIA_1m,aes(d,group=1)) +
  geom_line(aes(y=Rho, colour = 'Implied Correlation')) +
  geom_line(aes(y=index_variance, colour = 'Index Realized variance')) +
  geom_line(aes(y=sigma_zero, colour = 'Index variance (0)')) +
  geom_line(aes(y=sigma_one, colour = 'Index variance (1)')) +
  labs(x="Date",y="value",title = 'Average (Implied) Portfolio Correlation (1-Month Window)') +
  scale_colour_brewer(palette ="Pastel1",direction = -1)

df_DJIA_3m$d = as.Date(df_DJIA_3m$d,'%Y-%m-%d')
ggplot(df_DJIA_3m,aes(d,group=1)) +
  geom_line(aes(y=Rho, colour = 'Implied Correlation')) +
  geom_line(aes(y=index_variance, colour = 'Index Realized variance')) +
  geom_line(aes(y=sigma_zero, colour = 'Index variance (0)')) +
  geom_line(aes(y=sigma_one, colour = 'Index variance (1)')) +
  labs(x="Date",y="value",title = 'Average (Implied) Portfolio Correlation (3-Month Window)') +
  scale_colour_brewer(palette ="Pastel2",direction = -1)

#---------------------------------------------------------------------------------------#
#find peak correlation and the time it occurs
df_DJIA_1m[df_DJIA_1m$Rho == max(df_DJIA_1m$Rho),1]
df_DJIA_3m[df_DJIA_3m$Rho == max(df_DJIA_3m$Rho),1]

#---------------------------------------------------------------------------------------#
