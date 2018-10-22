library('csv')
library('dplyr')
library('readxl')
library('aTSA')
library('outliers')
library('quantmod')
library('ggplot2')
library('scales')
library('xtable')

setwd("~/Desktop/15.458 Financial Data Science/PS3")
dt <- read.csv("Q3return.csv",header=TRUE)
colnames(dt) = c('Date','Long','Short','Combined')
dt$Date = as.Date(dt$Date,"%m/%d/%Y")
df.fama = read.table("Q3FamaFrench(05-09).txt", sep = ',', header = TRUE)
colnames(df.fama) = c('Date','Risk Free Rate', 'Market Excess Return',
                       'SMB', 'HML', 'UMD')
df.fama$Date = as.Date(df.fama$Date,"%Y-%m-%d")

#---------------------------------------------------------------------------------------#
#1a) annualized return, volatility and SR, assume log return for long-short strategy
 
f <- function(x){
  ann_return = mean(x)*252
  vol =  sqrt(252)*sd(x)
  sr = ann_return/vol
  c(ann_return,vol,sr)
}

df.a = apply(dt[,-1],2,f)
rownames(df.a) = c('Annualized Return', 'Volatility', 'Sharpe Ratio')

#2b) CAPM
df = df= merge(dt,df.fama, by = "Date")
answer2b = summary(lm((df$`Combined`-df$`Risk Free Rate`)~df$`Market Excess Return`))


#2d)
plot(sort(dt$Combined), type = 'h',ylab = 'Return', xlab = 'Counts')

frac_winners =  length(which(dt$Combined>0))/length(dt$Combined)
frac_losers = length(which(dt$Combined<0))/length(dt$Combined)
median_winners = median( dt$Combined[which(dt$Combined>0)])
median_losers = median( dt$Combined[which(dt$Combined<0)])
df.d = data.frame(c(frac_winners,median_winners),c(frac_losers,median_losers))
colnames(df.d) = c('Winners', 'Losers')
rownames(df.d) = c('Fracation', 'Median')


