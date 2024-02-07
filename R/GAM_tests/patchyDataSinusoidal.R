library(mgcv)
library(tidyverse)

maxPointDensity <- 10  # amount of points present in regular periods

set.seed(42)

# time index
times <- seq(1,120)

# lambda for Poisson distributed count through time
true_lambda <- round((2 + sin(times/30)) * 5, digits = 1)

# we generate all true random draws prior to applying biases and observations failures
observations <- (sapply(true_lambda, rpois, n = maxPointDensity) 
  %>% as_tibble(names_to = NULL))

# reduces the number of observations for a certain period of time. Unbiased removal.
limitObservations <- function(observations, newObservationLimit, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  return( observations %>% 
            mutate( across(
              any_of(leftEnd:rightEnd), 
              ~if_else(row_number() > newObservationLimit, NA, .)
            )))
}

# removes observations based on the `removalCriteria`, a function that can be used
# to impose some bias
makePatches <- function(observations, removalCriteria, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  return( observations %>% 
            mutate( across(
              any_of(leftEnd:rightEnd), 
              ~if_else(removalCriteria(.), NA, .)
            ))
  )
}

# biased removal criterion that removes 50% of values above 10
removalCriteria1 <- function(x) {
  return(runif(1) < 0.5 & x > 10)
}

# apply removals to original observations to create new patchy observations
patchy_observations <- (observations 
  %>% makePatches(removalCriteria1)
  %>% limitObservations(8, 60, 79)
)

# put together data
data <- (data.frame(times, t(patchy_observations)) 
         %>% pivot_longer(cols = 2:(maxPointDensity+1), names_to = NULL))


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
