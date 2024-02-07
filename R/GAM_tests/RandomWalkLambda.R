## Example GAM with poisson distribution, where true Lambda deviates as a random walk over time
set.seed(42)

series_length <- 100
skip <- 10
data_points_per_time <- 10

# Generates a random walk of size (length*skip). Takes every (skip)th element.
# If skip is 10, it takes every 10th element of the random walk.
random_walk <- function(length, skip) {
  steps <- sample(c(-1, 1), length*skip, replace = TRUE)
  
  walk <- cumsum(steps)
  
  return(walk[seq(skip, length*skip, by=skip)])
}

times <- seq(1, series_length)
true_lambda <- abs(random_walk(series_length, skip))
plot(true_lambda)

obs <- sapply(true_lambda, rpois, n = data_points_per_time)

# put together data
dat <- data.frame(times, t(obs)) %>% pivot_longer(cols = 2:(data_points_per_time+1), names_to = NULL)
# check
plot(dat$times,dat$value)

# fit model
m <- gam(value~s(times, k=60),data=dat)

summary(m)
plot(m, residuals=TRUE)

gam.check(m)

# plot against data
m.pred <- predict.gam(m, newdata = data.frame(times))

plot(dat$times,dat$value, pch=19, cex=0.25)
points(times,true_lambda, pch=7, col="red")
points(times,m.pred, col = "blue")

# While GAMs can seem to fit well to Random Walks, their smooths require larger k 
# (Maximum allowed wiggliness)
