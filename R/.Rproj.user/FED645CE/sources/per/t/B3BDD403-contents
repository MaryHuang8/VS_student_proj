library(tidyverse)
library(dplyr)

source("seasonalities2.R")
source("dataOperators.R")
set.seed(42)

PERIOD <- 731
p <- new.env()

days <- 1:PERIOD
p$maxPointDensity <- 10

p$data <- generateSeasonalitiesData(days)
p$holidayAdjustment <- (
  p$data$publicHoliday + p$data$schoolHoliday
) * 0.35 + 1

p$weekdates <- data.frame(values = p$data$weekdate)
p$weekdates[p$weekdates == "Monday"]    <- rnorm(1, mean=1.0, sd=0.05)
p$weekdates[p$weekdates == "Tuesday"]   <- rnorm(1, mean=1.0, sd=0.05)
p$weekdates[p$weekdates == "Wednesday"] <- rnorm(1, mean=1.0, sd=0.05)
p$weekdates[p$weekdates == "Thursday"]  <- rnorm(1, mean=1.0, sd=0.05)
p$weekdates[p$weekdates == "Friday"]    <- rnorm(1, mean=1.2, sd=0.20)
p$weekdates[p$weekdates == "Saturday"]  <- rnorm(1, mean=1.0, sd=0.05)
p$weekdates[p$weekdates == "Sunday"]    <- rnorm(1, mean=0.8, sd=0.10)
p$weekdateAdjustment <- pmax(as.numeric(p$weekdates$values), 0.5)

p$monthAdjustment <- 0.990 + 0.001 * (p$data$month - 6) ^ 2

p$lockdown <- as.integer(p$data$lockdown)
p$boundary_indices <- which(diff(p$lockdown) == -1)
p$lockdownAdjustment <- 1 - 0.8 * p$lockdown

p$sequences_to_add <- 0:9

# Modify the original vector by adding consecutive sequences
modified_vectors <- lapply(p$sequences_to_add, function(seq) p$boundary_indices + seq)

# Concatenate all modified vectors into a single vector
p$result_vector <- sort(intersect(do.call(c, modified_vectors), which(!(p$data$lockdown))))
p$lockdownAdjustment[p$result_vector] <- rnorm(length(p$result_vector), mean=1.5, sd=0.1)

p$temperatureAdjustment <- ((p$data$temperature - mean(p$data$temperature))
                            / sd(p$data$temperature))
p$temperatureAdjustment <- 0.5 * p$temperatureAdjustment + 1

p$seasonalitiesAdjustment <- (
  p$holidayAdjustment * 
  p$weekdateAdjustment * 
  p$monthAdjustment * 
  p$lockdownAdjustment *
  p$temperatureAdjustment
)

p$trueMean <- pmax(8 * p$seasonalitiesAdjustment + rnorm(PERIOD, mean=4), 2)

# generate draws
p$observations <- (sapply(p$trueMean, rpois, n = p$maxPointDensity) 
                 %>% as_tibble(names_to = NULL))

# biased removal criterion that removes 20% of values above 20
p$removalCriteria1 <- function(x) {
  return(runif(1) < 0.2 & x > 20)
}

# biased modifier that caps maximum rate to 30
p$operator1 <- function(x) {
  pmin(x, 30)
}

# apply removals to original observations to create new patchy observations
p$patchy_observations <- (p$observations 
                        %>% makePatches(p$removalCriteria1)
                        %>% operate(p$operator1)
                        %>% limitObservations(5, 0, 40)
                        %>% limitObservations(8, 40, 400)
)

p$unObservedData <- (data.frame(
  p$data, t(p$observations)
)
  %>% pivot_longer(cols = 9:(8+p$maxPointDensity), names_to = NULL)
)
names(p$unObservedData)[9] <- "contacts"

# put together p$data
p$observedData <- (data.frame(
  p$data, t(p$patchy_observations)
) 
  %>% pivot_longer(cols = 9:(8+p$maxPointDensity), names_to = NULL)
)
names(p$observedData)[9] <- "contacts"
p$observedData <- p$observedData[complete.cases(p$observedData["contacts"]), ]

getSeasonalities <- function() {
  return(p$data)
}

getObservedData <- function() {
  return(p$observedData)
}

getUnobservedData__ <- function() {
  return(p$unObservedData)
}

getTrueMean__ <- function() {
  return(p$trueMean)
}