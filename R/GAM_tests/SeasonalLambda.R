library(mgcv)
library(tidyverse)

maxPointDensity <- 10  # amount of points present in regular periods

YEAR_LENGTH <- 365.2425
PERIOD <- as.integer(2 * YEAR_LENGTH)

set.seed(42)

# time index
times <- seq(1, PERIOD)

# yearly and weekly values are seasonalities caused by the day of the year, and
# the day of the week, respectively
yearly_values <- 1 + sin(2*pi * times / YEAR_LENGTH) / 5.0

holiday_values <- c(
  rep(1.3, 30),
  rep(1.0, 58),
  rep(1.3, 14),
  rep(1.0, 77),
  rep(1.3, 14),
  rep(1.0, 70),
  rep(1.3, 14),
  rep(1.0, 73),
  rep(1.3, 15)
)
holidays <- rep(holiday_values, length.out=PERIOD)

yearly <- yearly_values * holidays

# An educated guess that Fridays and Saturdays would lead to higher rates than
# the other weekdays
weekly_values <- c(1.0, 1.0, 1.0, 1.0, 1.5, 1.8, 0.9)
weekly <- rep(weekly_values, length.out=PERIOD)

# modelling government intervention as a series of lockdowns
lockdowns <- c(
  rep(1.0, 90), 
  rep(0.4, 43), 
  rep(0.8, 57), 
  rep(0.4, 111), 
  rep(0.8, 109),
  rep(0.4, 5),
  rep(0.8, 99),
  rep(0.4, 14),
  rep(0.8, 35),
  rep(0.4, 12),
  rep(0.8, 10),
  rep(0.4, 77),
  rep(0.8, 68)
)


# lambda for Poisson distributed count through time
true_lambda <- 10 * weekly * yearly * lockdowns
plot(times, true_lambda)

# generate draws
observations <- (sapply(true_lambda, rpois, n = maxPointDensity) 
                 %>% as_tibble(names_to = NULL))

# for this scenario, we ignore patchiness for now
patchy_observations <- observations

# put together data
data <- (data.frame(times, t(patchy_observations)) 
         %>% pivot_longer(cols = 2:(1+maxPointDensity), names_to = NULL))
plot(data$times,data$value)

# fit model
m <- gam(value~s(times, k=120), 
         data, 
         family = poisson(link = "log"), 
         method = 'REML')
summary(m)

gam.check(m)


plot(m, residuals=TRUE)
points(times,log(true_lambda) - mean(log(true_lambda)), cex=0.5, col="red")
# plot against data

plot(data$times,data$value, pch=19, cex=0.25)
points(times,true_lambda, pch=3, col="red")
m.pred <- exp(predict.gam(m, newdata = data.frame(times)))
points(times,m.pred, col = "blue")