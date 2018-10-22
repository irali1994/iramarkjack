#import data

setwd('~/2 ?????? Academics/Finance/15.458 & 459 Financial Data Science and Computing/Projects/Project C/Q4')


dt4_e_r <- read.csv("Q4d1 - Contra01-GICSfinancial-Weights&Returns.csv",header=TRUE)
dt4_e_mkt <- read.csv("Q3 - FamaFrench (05-09).txt")
#clean the data
dt4_e_mkt$d = as.Date(dt4_e_mkt$d,"%Y-%m-%d")
dt4_e_r$Trade.Date = as.Date(dt4_e_r$Trade.Date,"%m/%d/%Y")

ols4_e = lm((dt4_e_r$Weighted.Return-dt4_e_mkt$rf_str) ~ (dt4_e_mkt$mkt_str-dt4_e_mkt$rf_str))
summary(ols4_e)
