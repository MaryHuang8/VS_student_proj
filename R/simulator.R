library(tidyverse)

source("R/seasonalities.R")
source("R/dataOperators.R")

PERIOD <- as.integer(2 * YEAR_LENGTH)
p <- new.env()

days <- 1:PERIOD
p$maxPointDensity <- 10

temperature <- sapply(days, generateTemperature)
p$temperatureAdjustment <- temperature / mean(temperature)

holiday <- as.integer(sapply(days, isSchoolHoliday))
p$holidayAdjustment  <- 1 + holiday * 0.3

lockdown <- as.integer(sapply(days, underLockdown))
p$lockdownAdjustment <- 1 - lockdown * 0.7

p$seasonalities <- p$temperatureAdjustment * p$holidayAdjustment * p$lockdownAdjustment

p$trueMean <- pmax(10 * p$seasonalities + rnorm(PERIOD), 1)

# generate draws
p$observations <- (sapply(p$trueMean, rpois, n = p$maxPointDensity) 
                 %>% as_tibble(names_to = NULL))

# biased removal criterion that removes 20% of values above 10
p$removalCriteria1 <- function(x) {
  return(runif(1) < 0.2 & x > 10)
}

# biased modifier that caps maximum rate to 18
p$operator1 <- function(x) {
  pmin(x, 18)
}

# apply removals to original observations to create new patchy observations
p$patchy_observations <- (p$observations 
                        %>% makePatches(p$removalCriteria1)
                        %>% operate(p$operator1)
                        %>% limitObservations(1, 0, 40)
                        %>% limitObservations(3, 40, 200)
)

p$unObservedData <- (data.frame(
  days, temperature, holiday, lockdown, t(p$observations)
)
  %>% pivot_longer(cols = 5:(4+p$maxPointDensity), names_to = NULL)
)
colnames(p$unObservedData) <- c(
  "day", "temperature", "holiday", "lockdown", "contacts"
)

# put together data
observedData <- (data.frame(
  days, temperature, holiday, lockdown, t(p$patchy_observations)
) 
  %>% pivot_longer(cols = 5:(4+p$maxPointDensity), names_to = NULL)
)
colnames(observedData) <- c(
  "day", "temperature", "holiday", "lockdown", "contacts"
)

getUnobservedData__ <- function() {
  return(p$unObservedData)
}

getTrueMean__ <- function() {
  return(p$trueMean)
}