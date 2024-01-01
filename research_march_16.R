install.packages("vars")
install.packages("TSstudio")
install.packages("rtweet")
install.packages("hablar")
install.packages("tseries")
install.packages("bruceR")

library(bruceR)
library(tseries)
library(rtweet)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(dplyr)
library(hablar)
library(readxl)

#loaded data set 
Japan_BOJ <- read_excel("Desktop/Research/Japan BOJ.xlsx", 
                        range = "A1:X435")
View(Japan_BOJ)
#removed N/A entry and checked
Japan_short <- Japan_BOJ[-c(1:174),]
glimpse(Japan_short)
#changed BDI from character to integer
Japan_cleaned <- Japan_short %>% convert(int(BDI))
head(Japan_cleaned$BDI)
glimpse(Japan_cleaned)


#created time series
INCO <- ts(Japan_cleaned[,6], start=c(1999,7), frequency = 12)
Ocean <- ts(Japan_cleaned[,3], start=c(1999,7), frequency = 12)
Rail <- ts(Japan_cleaned[,12], start=c(1999,7), frequency = 12)
Air <- ts(Japan_cleaned[,9], start=c(1999,7), frequency = 12)
BDI <- ts(Japan_cleaned[,21], start=c(1999,7), frequency = 12)
Road <- ts(Japan_cleaned[,13], start=c(1999,7), frequency = 12)
#visual check
autoplot(INCO)
autoplot(Ocean)
autoplot(Rail)
autoplot(Air)
autoplot(BDI)
autoplot(Road)
#stationary test level
adf.test(log(INCO))
adf.test(log(Ocean))
adf.test(log(Rail))
adf.test(log(Air))
adf.test(log(BDI))
adf.test(log(Road))
pp.test(log(INCO))
pp.test(log(Ocean))
pp.test(log(Rail))
pp.test(log(Air))
pp.test(log(BDI))
pp.test(log(Road))

#first differences
d_lninco <- diff(log(INCO))
d_lnocean <-diff(log(Ocean))
d_lnrail <-diff(log(Rail))
d_lnair <-diff(log(Air))
d_lnbdi <-diff(log(BDI))
d_lnroad <-diff(log(Road))
#Visualizing first differences
autoplot(d_lninco)
autoplot(d_lnocean)
autoplot(d_lnrail)
autoplot(d_lnbdi)
autoplot(d_lnbdi)
autoplot(d_lnroad)


#stationary test first diff 
adf.test(d_lninco)
adf.test(d_lnocean)
adf.test(d_lnrail)
adf.test(d_lnair)
adf.test(d_lnbdi)
adf.test(d_lnroad)
pp.test(d_lninco)
pp.test(d_lnocean)
pp.test(d_lnrail)
pp.test(d_lnair)
pp.test(d_lnbdi)
pp.test(d_lnroad)

#built VAR model
var_all <- cbind(d_lninco, d_lnocean, d_lnrail, d_lnair, d_lnbdi, d_lnroad)

#lag selection for all variables; AIC
lagselect_all <- VARselect(var_all, lag.max = 15, type = "const")
lagselect_all$selection

#var all
model_all <- VAR(var_all, p = 4, type = "const", season = NULL, exog = NULL) 
summary(model_all)

#Granger analysis; one to all
granger_causality(
  model_all,
  var.y = NULL,
  var.x = NULL,
  test = ("Chisq"),
  file = NULL,
  check.dropped = FALSE
)


