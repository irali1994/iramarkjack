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
dt <- read.csv("Q2return.csv",header=TRUE)
colnames(dt) = c('Date','Return')
dt$Date = as.Date(dt$Date,"%m/%d/%Y")
df.fama = read.table("Q2FamaFrench(01-04).txt", sep = ',', header = TRUE)
colnames(df.fama) = c('Date','Risk Free Rate', 'Market Excess Return',
                      'SMB', 'HML', 'UMD')
df.fama$Date = as.Date(df.fama$Date,"%Y/%m/%d")

#---------------------------------------------------------------------------------------#
#1a) annualized return, volatility and SR, assume log return

ann_return = mean(dt$Return)*252
vol =  sqrt(252)*sd(dt$Return)
sr = ann_return/vol
df.a = data.frame(ann_return,vol,sr)
colnames(df.a) = c('Annualized Return', 'Volatility', 'Sharpe Ratio')

#2b) CAPM

answer2b = summary(lm((dt$`Return`-df.fama$`Risk Free Rate`)~df.fama$`Market Excess Return`)
)

#2c) 

answer2c = summary(lm((dt$`Return`-df.fama$`Risk Free Rate`)~df.fama$`Market Excess Return`+df.fama$SMB + df.fama$HML + df.fama$UMD)
)

#2d)

plot(sort(dt$Return), type = 'h',ylab = 'Return', xlab = 'Counts')

frac_winners =  length(which(dt$Return>0))/length(dt$Return)
frac_losers = length(which(dt$Return<0))/length(dt$Return)
median_winners = median( dt$Return[which(dt$Return>0)])
median_losers = median( dt$Return[which(dt$Return<0)])
df.d = data.frame(c(frac_winners,median_winners),c(frac_losers,median_losers))
colnames(df.d) = c('Winners', 'Losers')
rownames(df.d) = c('Fracation', 'Median')

