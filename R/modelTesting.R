library(mgcv)
library(tidyverse)

source("R/simulator.R")
data <- observedData

plot(data$day,data$contacts, pch=19, cex=0.25)

# Smooth of day only
mDay <- gam(contacts~s(day, k=100), 
         data,
         family = poisson(link = "log"), 
         method = 'REML')
summary(mDay)

gam.check(mDay, rep=500)

plot(data$day,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mDay, newdata = data.frame(day=days)))
points(days, pred, col="blue")

#Smooth of day + lockdown
mLockdown <- gam(contacts~s(day, k=100, by=lockdown) + lockdown, 
         data,
         family = poisson(link = "log"), 
         method = 'REML')
summary(mLockdown)

gam.check(mLockdown)

plot(data$day,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mLockdown, newdata = data.frame(day=days, lockdown)))
points(days, pred, col="blue")

#Smooth of day + lockdown + holiday
mHoliday <- gam(contacts~s(day, k=100) + lockdown + holiday, 
                 data,
                 family = poisson(link = "log"), 
                 method = 'REML')
summary(mHoliday)

gam.check(mHoliday)

plot(data$day,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mHoliday, newdata = data.frame(day=days, lockdown, holiday)))
points(days, pred, col="blue")