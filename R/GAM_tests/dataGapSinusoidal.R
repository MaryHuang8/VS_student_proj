library(mgcv)
library(tidyverse)

points_dense <- 10  # amount of points present in regular periods
points_sparse <- 1  # amount of points present in 'reporting gaps'

set.seed(42)

# time index
times <- seq(1,120)

# lambda for Poisson distributed count through time
true_lambda <- round((2 + sin(times/30)) * 5, digits = 1)

# generate draws
observations <- (sapply(true_lambda, rpois, n = points_dense) 
  %>% as_tibble(names_to = NULL))

clear_points <- function(observations, leftEnd, rightEnd, newPointDensity) {
  return( observations %>% 
    mutate( across(
        any_of(leftEnd:rightEnd), 
        ~if_else(row_number() > newPointDensity, NA_integer_, .)
    )))
}

patchy_observations <- observations %>% clear_points(20, 60, points_sparse)

# put together data
data <- (data.frame(times, t(patchy_observations)) 
         %>% pivot_longer(cols = 2:(1+points_dense), names_to = NULL))


# check
plot(data$times,data$value)

# fit model
m <- gam(value~s(times), 
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
pred <- exp(predict.gam(m, newdata = data.frame(times)))
points(times,pred, col = "blue")

prediction <- predict(m, newdata = data.frame(times), type = "response", se.fit = TRUE)
plot(times, prediction[1])
