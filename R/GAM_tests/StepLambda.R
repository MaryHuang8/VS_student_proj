## Example GAM with poisson distribution, where true Lambda jumps and drops
set.seed(42)
data_points_per_time <- 10


true_lambda <- c(rep(10, times=20), rep(20, times=10), rep(10, times=20))
times <- seq(1, length(true_lambda))

plot(true_lambda)

obs <- sapply(true_lambda, rpois, n = data_points_per_time)

# put together data
dat <- data.frame(times, t(obs)) %>% pivot_longer(cols = 2:(data_points_per_time+1), names_to = NULL)
# check
plot(dat$times,dat$value)

# fit model
m <- gam(value~s(times, k=40),data=dat, family = poisson(link = "log"))

summary(m)
plot(m, residuals=TRUE)

gam.check(m)

plot(m, residuals=TRUE)
points(times,log(true_lambda) - mean(log(true_lambda)), cex=0.5, col="red")

# plot against data
m.pred <- predict.gam(m, newdata = data.frame(times))

plot(dat$times,dat$value, pch=19, cex=0.25)
points(times,true_lambda, pch=3, col="red")
points(times,m.pred, col = "blue")
