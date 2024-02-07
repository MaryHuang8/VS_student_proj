library(mgcv)
library(tidyverse)

maxPointDensity <- 5  # amount of points present in regular periods

YEAR_LENGTH <- 365.2425
PERIOD <- as.integer(YEAR_LENGTH)

set.seed(40)

generate_seasonalities <- function(period) {
  # time index
  times <- seq(1, period)
  
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
  holidays <- rep(holiday_values, length.out=period)
  
  yearly <- yearly_values * holidays
  
  # An educated guess that Fridays and Saturdays would lead to higher rates than
  # the other weekdays
  weekly_values <- c(1.0, 1.0, 1.0, 1.0, 1.5, 1.8, 0.9)
  weekly <- rep(weekly_values, length.out=period)
  
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
    rep(0.8, 68),
    rep(1.0, each= Inf)
  )[1:period]
  
  return(weekly * yearly * lockdowns)
}

random_walk <- function(period, skip=1) {
  steps <- sample(c(-1, 1), period*skip, replace = TRUE)
  
  walk <- cumsum(steps)
  
  return(walk[seq(skip, period*skip, by=skip)])
}

times <- seq(1, PERIOD)
true_mean <- 
  2 * generate_seasonalities(PERIOD) * (1+sqrt(abs(random_walk(PERIOD)))) + rnorm(PERIOD)
true_mean <- pmax(1, true_mean)

plot(times, true_mean)

# generate draws
observations <- (sapply(true_mean, rpois, n = maxPointDensity) 
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

# mutates `observations` as per operator
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
                        %>% limitObservations(1, 0, 40)
                        %>% limitObservations(3, 40, 200)
)

# put together data
data <- (data.frame(times, t(patchy_observations)) 
         %>% pivot_longer(cols = 2:(1+maxPointDensity), names_to = NULL))
plot(data$times,data$value)

# fit model
m <- gam(value~s(times, k=100), 
         data,
         family = poisson(link = "log"), 
         method = 'REML')
summary(m)

gam.check(m)


plot(m, residuals=TRUE)
points(times,log(true_mean) - mean(log(true_mean)), cex=0.5, col="red")
# plot against data

plot(data$times,data$value, pch=19, cex=0.25)
points(times,true_mean, pch=3, col="red")
m.pred <- exp(predict.gam(m, newdata = data.frame(times)))
points(times,m.pred, col = "blue")