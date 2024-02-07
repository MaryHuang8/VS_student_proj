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

# reduces the number of observations for a certain period of time. Biases
# can be imposed depending on where data is removed from
limitObservations <- function(observations, newObservationLimit, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  observations %>% 
    mutate( 
      across(
        any_of(leftEnd:rightEnd), 
        ~if_else(row_number() > newObservationLimit, NA, .)
      )
    )
}

# removes observations based on the `removalCriteria`, a function that can be used
# to impose some bias
makePatches <- function(observations, removalCriteria, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  observations %>% 
    mutate( 
      across(
        any_of(leftEnd:rightEnd), 
        ~if_else(removalCriteria(.), NA, .)
      )
    )
}

operate <- function(observations, operator, leftEnd = 0, rightEnd = NULL) {
  if (is.null(rightEnd)) rightEnd <- dim(observations)[2]
  
  observations %>% 
    mutate( 
      across(
        any_of(leftEnd:rightEnd), 
        ~operator(.)
      )
    )
}

# biased removal criterion that removes 20% of values above 10
removalCriteria1 <- function(x) {
  return(runif(1) < 0.2 & x > 10)
}

# biased modifier that caps maximum rate to 18
operator1 <- function(x) {
  pmin(x, 18)
}

# apply removals to original observations to create new patchy observations
patchy_observations <- (observations 
  %>% makePatches(removalCriteria1)
  %>% operate(operator1)
  %>% limitObservations(3, 0, 40)
  %>% limitObservations(7, 40, 60)
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
m.pred <- exp(predict.gam(m, newdata = data.frame(times)))
points(times,m.pred, col = "blue")
