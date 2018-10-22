library('csv')
library('dplyr')
library('readxl')
library('aTSA')
library('outliers')
library('quantmod')
library('ggplot2')

setwd('C:/Users/zheng/Documents/2 ?????? Academics/Finance/15.458 & 459 Financial Data Science and Computing/Projects/Project C/Q1')
rawdata <- read.csv("Query1.csv",header = T)

SPX <- c()
SPTR <- c()
VIX <- c()
GS <- c()
Date <- c()

t <- 1

for(i in 1:nrow(rawdata))
{
  if(as.character(rawdata$dbo_bench_ticker[i])=="SPX")
  {
    SPX[t] <- as.numeric(rawdata$dbo_benchvalue_value[i])
    GS[t] <- as.numeric(rawdata$dbo_dailyvalue_value[i])
    Date[t] <- as.character(rawdata$d[i])
  }
  if(as.character(rawdata$dbo_bench_ticker[i])=="SPTR")
  {
    SPTR[t] <- as.numeric(rawdata$dbo_benchvalue_value[i])
  }
  if(as.character(rawdata$dbo_bench_ticker[i])=="VIX")
  {
    VIX[t] <- as.numeric(rawdata$dbo_benchvalue_value[i])
    t <- t+1
  }
}

deNormRet <- data.frame(Date,SPX,SPTR,VIX,GS)

#=========================================================================================

dt <- read.csv("C:/Users/zheng/Downloads/Daily return(beta).csv",header=TRUE)
dt$Date = as.Date(dt$Date,"%Y/%m/%d")
colnames(dt) = c('Date','SPX','SPTR','VIX','GS','beta')


#check data integrity
sum(is.na(dt$GS))
sum(is.na(dt$SPX))
sum(is.na(dt$SPTR))
sum(is.na(dt$VIX))
sum(is.na(dt$beta))

#---------------------------------------------------------------------------------------#

#1a)scatter plot of beta vs. SPX
scatterplot_a = ggplot(dt, aes(x=Date, y=beta)) + geom_line()
scatterplot_a = scatterplot_a  +labs(x = "Date", y='Stock Beta', title='time series plot of the GS beta')
scatterplot_a

#---------------------------------------------------------------------------------------#

#1b)scatter plot of GS return vs. SPTR
scatterplot_b = ggplot(dt, aes(x=SPTR, y=GS)) + geom_point()
scatterplot_b = scatterplot_b  +labs(x = "S&P 500 total return index",y='Stock return', title='Scatter plot of the GS return vs. SPTR')
scatterplot_b 
# Add the regression line
scatterplot_b + geom_smooth(method=lm)

#linear regression equation
ols_b = lm(dt$GS ~ dt$SPTR)
summary(ols_b)

#---------------------------------------------------------------------------------------#

#1c)scatter plot of GS return vs. VIX
scatterplot_c = ggplot(dt, aes(x=GS, y=VIX)) + geom_point()
scatterplot_c  = scatterplot_c  +labs(x = "VIX",y='Stock return', title='Scatter plot of the GS return vs. VIX')
scatterplot_c 
# Add the regression line
scatterplot_c + geom_smooth(method=lm)

#linear regression equation
ols_c = lm(dt$VIX ~ dt$GS)
summary(ols_c)





