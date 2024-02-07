# # install packages if not installed
# install.packages("mgcv") # package to fit GAM
# install.packages("tidyverse") # for data transformation and plotting

library(mgcv)
library(tidyverse)

# default gam example
set.seed(2) ## simulate some data...
dat <- gamSim(1,n=400,dist="normal",scale=2)
b <- gam(y~s(x0)+s(x1)+s(x2, k=20)+s(x3),data=dat)
summary(b)
plot(b,pages=1,residuals=TRUE)  ## show partial residuals
plot(b,pages=1,seWithMean=TRUE) ## `with intercept' CIs
## run some basic model checks, including checking
## smoothing basis dimensions...
gam.check(b)


# time series example

# time index
times <- seq(1,180)

# lambda for Poisson distributed count through time
true_lambda <- round((2 + sin(times/30)) * 5, digits = 1)

# generate draws
set.seed(42)
obs <- sapply(true_lambda,rpois, n = 10)

# put together data
dat <- data.frame(times, t(obs)) %>% pivot_longer(cols = 2:11, names_to = NULL)
# check
plot(dat$times,dat$value)

# fit model
m <- gam(value~s(times),data=dat)
summary(m)
plot(m, residuals=TRUE)
# plot against data
m.pred <- predict.gam(m, newdata = data.frame(times))

plot(dat$times,dat$value)
points(times,m.pred, col = "red")
gam.c
