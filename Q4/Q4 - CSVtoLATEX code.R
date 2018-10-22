library(xtable)

setwd('~/2 ?????? Academics/Finance/15.458 & 459 Financial Data Science and Computing/Projects/Project C/Q4')


df_Q3e <- read.csv('Q3e - Contra01-2006-Returns-SICsector - FlatFile.csv')
xtable(df_Q3e, digits=3)

df_Q3f <- read.csv('Q3f - Contra01-2006-Returns-GICSsector - FlatFile.csv')
xtable(df_Q3f, digits=3)
View(df_Q3f)

df_Q4a <- read.csv('Q4a - Contra01-GICSsector-Weights - 2005-2009 - Max&Min - FlatFile.csv')
xtable(df_Q4a, digits=3)
View(df_Q4a)

df_Q4b <- read.csv('Q4b - Contra01-GICSsector-Weights - 20080915 - FlatFile.csv')
xtable(df_Q4b, digits=3)
View(df_Q4b)

df_Q4c <- read.csv('Q4c - Contra01-GICSsector-Weights - 20070727 - FlatFile.csv')
xtable(df_Q4c, digits=3)
View(df_Q4c)


