library(mgcv)
library(tidyverse)
library(expss)
library(gridExtra) # coplot

source("simulator2.R")
source("crossValidationFunctions.R");
source("plotModel.R")
data <- getObservedData()
seasonalitiesData <- getSeasonalities()

#````````````````````````````````````````````````````````````
# Initial data manipulation
#````````````````````````````````````````````````````````````

# Convert weekdate to weekends to simplify parameters````````
weekdateToWeekend <- function(weekdate) {
  weekdate[weekdate == "Monday" | weekdate == "Tuesday" | weekdate == "Wednesday" 
           | weekdate == "Thursday" | weekdate == "Friday"] <- FALSE
  weekdate[weekdate == "Saturday" | weekdate == "Sunday"] <- TRUE
  return (as.logical(weekdate))
}
data$weekdate <- weekdateToWeekend(data$weekdate)
seasonalitiesData$weekdate <- weekdateToWeekend(seasonalitiesData$weekdate)

# Convert categorical variables to factors to use as potential by variable for smoothing term
#data$weekdate <- factor(data$weekdate, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), ordered = TRUE)
data$weekdate <- as.factor(data$weekdate)
data$lockdown <- as.factor(data$lockdown)
data$schoolHoliday <- as.factor(data$schoolHoliday)
data$publicHoliday <- as.factor(data$publicHoliday)

#``````````````````````````````````````````````````````````````````````````````
# Evaludating model performance
#`````````````````````````````````````````````````````````````````````````````

# Single predictors````````````````````````````````````````````````````````````
train <- function(data) {
  gam(contacts~s(days, k=15), 
      data, 
      family = poisson(link = "log"), 
      method = 'REML')
}

singlePredVSSmoothedModels <- list(mDays, mLockdown, mWeekdate, mSchoolHoliday, mPublicHoliday, mMonth, mYear, mSmoothDay)
singlePredVSSmoothedModelTitles <- list("Days", "Lockdown", "Weekdate", "School Holiday", "Public Holiday", "Month", "Year", "Smooth of Day")
ggPlotModelPredictions(singlePredVSSmoothedModels, singlePredVSSmoothedModelTitles, "Single Predictor VS Smoothed Model Predictions", 2, 4);


# Cross validation visualisation```````````````````````````````````````````````

train <- function(data) {
  return (gam(contacts ~ s(days, by = lockdown, k = 20) + 
                s(temperature, k = 5) + weekdate + schoolHoliday + publicHoliday,
              data = data,
              family = poisson(link = "log"))
  )
}

test <- function(model, data) {
  return(predict(model, newdata=data, type="response") )
}

evaluate <- function(data, pred) {
  agg_data <- aggregate(data.frame(data$days, data$contacts, pred), by = list(data$days), FUN = mean)
  colnames(agg_data)[2] <- "days"
  colnames(agg_data)[3] <- "contacts"
  colnames(agg_data)[4] <- "pred"
  return(sqrt(mean((agg_data$contacts - agg_data$pred)^2)))
}

partition <- function(data) {
  return (generatePartitionsEvenData(data, k=5))
}

crossValidateGam(data = data,
                 partition,
                 train,
                 test,
                 evaluate
)

crossValidateGamPlot(data = data,
                     partition,
                     train,
                     test,
                     evaluate,
                     "",
                     "k=20", #modify this according to k value in train
                     "Day"
)

# Knots tuning using cross validation with visualisation````````````````````

changeSmoothDayOnlyModelKnots <- function(data, nKnots) {
  return(gam(contacts ~ s(days, k = nKnots),
             data = data,
             family = poisson(link = "log"),
             method="REML"))
}

changeFinalModelKnots <- function(data, nKnots) {
  return(gam(contacts ~ s(days, by = lockdown, k = nKnots) + 
               s(temperature, k = 10) + weekdate + schoolHoliday + publicHoliday,
             data = data,
             family = poisson(link = "log")))
}

knotsTuningPlot(data, partition, changeSmoothDayOnlyModelKnots, test, evaluate,
                2, 50);

knotsTuningPlot(data, partition, changeFinalModelKnots, test, evaluate, 2, 50);

#````````````````````````````````````````````````````````````````````````````
# Model Testing
#````````````````````````````````````````````````````````````````````````````

# Preliminary data pattern observations ``````````````````````````````````````

plot(data$days,data$contacts, pch=19, cex=0.25)

mDays <- gam(contacts ~ days,
             data,
             family = poisson(link="log"),
             method = 'REML');
summary(mDays)

gam.check(mDays, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mDays, newdata = seasonalitiesData))
points(days, pred, col="blue")

# publicHoliday
mPublicHoliday <- gam(contacts ~ publicHoliday,
                      data,
                      family = poisson(link="log"),
                      method = 'REML');
summary(mPublicHoliday)

gam.check(mPublicHoliday, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mPublicHoliday, newdata = seasonalitiesData))
points(days, pred, col="blue")

# schoolHoliday
mSchoolHoliday <- gam(contacts ~ schoolHoliday,
                      data,
                      family = poisson(link="log"),
                      method = 'REML');
summary(mSchoolHoliday)

gam.check(mSchoolHoliday, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSchoolHoliday, newdata = seasonalitiesData))
points(days, pred, col="blue")


# weekdate
mWeekdate <- gam(contacts ~ weekdate,
                 data,
                 family = poisson(link="log"),
                 method = 'REML');
summary(mWeekdate)

gam.check(mWeekdate, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mWeekdate, newdata = seasonalitiesData))
points(days, pred, col="blue")

# month
mMonth <- gam(contacts ~ month,
              data,
              family = poisson(link="log"),
              method = 'REML');
summary(mMonth)

gam.check(mMonth, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mMonth, newdata = seasonalitiesData))
points(days, pred, col="blue")

# year
mYear <- gam(contacts ~ year,
             data,
             family = poisson(link="log"),
             method = 'REML');
summary(mYear)

gam.check(mYear, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mYear, newdata = seasonalitiesData))
points(days, pred, col="blue")

# temperature
mTemperature <- gam(contacts ~ temperature,
                    data,
                    family = poisson(link="log"),
                    method = 'REML');
summary(mTemperature)

gam.check(mTemperature, rep=500)

plot(data$days, data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mTemperature, newdata = seasonalitiesData))
points(days, pred, col="blue")


#lockdown only
mLockdown <- gam(contacts ~ lockdown,
                 data,
                 family = poisson(link="log"),
                 method = 'REML');
summary(mLockdown)

gam.check(mLockdown, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mLockdown, newdata = seasonalitiesData))
points(days, pred, col="blue")

singlePredModels <- list(mDays, mLockdown, mWeekdate, mSchoolHoliday, mPublicHoliday, mMonth, mYear)
singlePredModelTitles <- list("Days", "Lockdown", "Weekdate", "School Holiday", "Public Holiday", "Month", "Year", "Temperature")
plotModelPredictions(singlePredModels, "Single Predictor Model Predictions", 2, 4)
ggPlotModelPredictions(singlePredModels, singlePredModelTitles, "Single Predictor Model Predictions", 2, 4);


singlePredVSSmoothedModels <- list(mDays, mLockdown, mWeekdate, mSchoolHoliday, mPublicHoliday, mMonth, mYear, mSmoothDay)
singlePredVSSmoothedModelTitles <- list("Days", "Lockdown", "Weekdate", "School Holiday", "Public Holiday", "Month", "Year", "Smooth of Day")
ggPlotModelPredictions(singlePredVSSmoothedModels, singlePredVSSmoothedModelTitles, "Single Predictor VS Smoothed Model Predictions", 2, 4);
#``````````````````````````````````````````````````````````
# Smooth & No interactions
#``````````````````````````````````````````````````````````

# Smooth of day only
mSmoothDay <- gam(contacts~s(days, k=15), 
         data,
         family = poisson(link = "log"), 
         method = 'REML')
summary(mSmoothDay)

train <- function(data) {
  gam(contacts~s(days, k=15), 
      data, 
      family = poisson(link = "log"), 
      method = 'REML')
}
crossValidateGam(data, partition, train, test, evaluate)
gam.check(mSmoothDay, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDay, newdata = seasonalitiesData))
points(days, pred, col="blue")

#`````````````````
# Smooth of day: spline = cubic regression
mSmoothDayCR <- gam(contacts~s(days, bs = 'cr', k=100), 
                  data,
                  family = poisson(link = "log"), 
                  method = 'REML')
summary(mSmoothDayCR)

gam.check(mSmoothDayCR, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayCR, newdata = seasonalitiesData))
points(days, pred, col="blue")


# Smooth of day: spline = thin plate regression
mSmoothDayTPR <- gam(contacts~s(days, bs = 'tp', k=100), 
                    data,
                    family = poisson(link = "log"), 
                    method = 'REML')
summary(mSmoothDayTPR)

gam.check(mSmoothDayTPR, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayTPR, newdata = seasonalitiesData))
points(days, pred, col="blue")

# Smooth of day: spline = p-spline
mSmoothDayPS <- gam(contacts~s(days, bs = 'ps', k=100), 
                    data,
                    family = poisson(link = "log"), 
                    method = 'REML')
summary(mSmoothDayPS)

gam.check(mSmoothDayPS, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayPS, newdata = seasonalitiesData))
points(days, pred, col="blue")

# Smooth of day: spline = random effects
mSmoothDayCCR <- gam(contacts~s(days, bs = 're', k=100), 
                     data,
                     family = poisson(link = "log"), 
                     method = 'REML')
summary(mSmoothDayCCR)

gam.check(mSmoothDayCCR, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

true_mean <- getTrueMean__()
points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayCCR, newdata = seasonalitiesData))
points(days, pred, col="blue")

#``````````````````

#Smooth of day + lockdown
mSmoothDayLockdown <- gam(contacts~s(days, k=100) + lockdown, 
                 data,
                 family = poisson(link = "log"), 
                 method = 'REML')
summary(mSmoothDayLockdown)

gam.check(mSmoothDayLockdown, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayLockdown, newdata = seasonalitiesData))
points(days, pred, col="blue")

#``````````````````````````````````````````````````````````

#Smooth of day + lockdown + holidays
mDayLockdownHoliday <- gam(contacts~s(days, k=100) + lockdown + publicHoliday + schoolHoliday, 
                data,
                family = poisson(link = "log"), 
                method = 'REML')
summary(mDayLockdownHoliday)

gam.check(mDayLockdownHoliday, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mDayLockdownHoliday, newdata = seasonalitiesData))
points(days, pred, col="blue")

#``````````````````````````````````````````````````````````

#Smooth of lockdown + holidays + day of the week!
mDayLockdownHolidayWeekdate <- gam(contacts~s(days, k=100) + 
                   lockdown + publicHoliday + schoolHoliday + weekdate, 
                 data,
                 family = poisson(link = "log"), 
                 method = 'REML')
summary(mDayLockdownHolidayWeekdate)

gam.check(mDayLockdownHolidayWeekdate)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mDayLockdownHolidayWeekdate, newdata = seasonalitiesData))
points(days, pred, col="blue")

#``````````````````````````````````````````````````````````

#Smooth of everything!
mSmoothDayEverything <- gam(contacts~s(days, k=100) + 
                     lockdown + publicHoliday + schoolHoliday
                   + weekdate + month + year + temperature, 
                   data,
                   family = poisson(link = "log"), 
                   method = 'REML')
summary(mSmoothDayEverything)

gam.check(mSmoothDayEverything, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayEverything, newdata = seasonalitiesData))
points(days, pred, col="blue")

# meanSquareError <- function(pred, true_mean) {
#   return sum((pred-true_mean)^2) / length(true_mean)
# }

#Smooth day and smooth month``````````````````````````````````````````````

mSmoothDaySmoothMonth <- gam(contacts~s(days, k=100) + s(month, k=12), 
                            data,
                            family = poisson(link = "log"), 
                            method = 'REML')
summary(mSmoothDaySmoothMonth)

gam.check(mSmoothDaySmoothMonth, rep=500)
#s(days) ***, s(month) not significant

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDaySmoothMonth, newdata = seasonalitiesData))
points(days, pred, col="blue")

plot(mSmoothDaySmoothMonth)

# Smooth day and smooth year ``````````````````````````````````
## too few years, doesn't work
mSmoothDaySmoothYear <- gam(contacts~s(days, k=100) + s(year, k=2), 
                            data,
                            family = poisson(link = "log"), 
                            method = 'REML')
summary(mSmoothDaySmoothYear)

gam.check(mSmoothDaySmoothYear, rep=500)
#s(days) ***, s(year) not significant

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(model, newdata = seasonalitiesData))
points(days, pred, col="blue")

plot(model)


# Every feature smoothed or included ````````````````````````````````````````````

# Convert categorical variables to factors
#data$weekdate <- factor(data$weekdate, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'), ordered = TRUE)
data$weekdate <- as.factor(data$weekdate)
data$lockdown <- as.factor(data$lockdown)
data$schoolHoliday <- as.factor(data$schoolHoliday)
data$publicHoliday <- as.factor(data$publicHoliday)

# Set a smaller number of knots for 'days'
num_knots_list <- list(
  days = 50,
  temperature = 10
)

# Fit GAM model
model <- gam(contacts ~ s(days, by = lockdown, k = num_knots_list$days) + 
               s(temperature, k = num_knots_list$temperature),
             data = data,
             family = poisson(link = "log"))

# Summary of the model
summary(model)

gam.check(model, rep=500)

# Diagnostic plots
par(mfrow=c(2,2))
plot(model)

# Prediction
predictions <- predict(model, newdata = data, type = "response")

# Plot observed vs. predicted values
plot(data$days, data$contacts, pch=19, cex=0.25, main="Observed vs Predicted")
lines(data$days, predictions, col="red")

# Visualize the smooth terms
par(mfrow=c(2,2))
plot(model, select = 1:5)

#cross validate
crossValidateGam(data = data,
                 partition,
                 gam(contacts ~ s(days, by = lockdown, k = num_knots_list$days) + 
                       s(temperature, k = num_knots_list$temperature),
                     data = data,
                     family = poisson(link = "log")),
                 test,
                 evaluate
)
# 92.29972
crossValidateGamPlot(data = data,
                     gam(contacts ~ s(days, by = lockdown, k = num_knots_list$days) + 
                           s(temperature, k = num_knots_list$temperature),
                         data = data,
                         family = poisson(link = "log")),
                     "Model Prediction During Cross Validation",
                     "Number of Contacts",
                     "Days"
)


#cross validate with categorical parameters
crossValidateGam(data = data,
                 partition,
                 gam(contacts ~ s(days, by = lockdown, k = num_knots_list$days) + 
                       s(temperature, k = num_knots_list$temperature) + weekdate + schoolHoliday + publicHoliday,
                     data = data,
                     family = poisson(link = "log")),
                 test,
                 evaluate
)
# 92.29972

knots_list <- c(10, 50, 100)

crossValidateGamPlot(data = data,
                     gam(contacts ~ s(days, by = lockdown, k = 39) + 
                           s(temperature, k = 10),
                         data = data,
                         family = poisson(link = "log")),
                     "",
                     "k=39",
                     "Days"
                     )

#``````````````````````````````````````````````````````````
# With interactions. Method: Use the same smoothed function to variables
#``````````````````````````````````````````````````````````
#Smooth Day and Smooth Month Interact
mSmoothDayMonthInteract <- gam(contacts~s(days, month),
                   data,
                   family = poisson(link = "log"), 
                   method = 'REML')
summary(mSmoothDayMonthInteract)
#R-sq.(adj) =  0.659   Deviance explained = 65.1%. BAD.

gam.check(mSmoothDayMonthInteract, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayMonthInteract, newdata = seasonalitiesData))
points(days, pred, col="blue")

#Smooth Day and Smooth Year Interact ``````````````````````````````
mSmoothDayYearInteract <- gam(contacts~s(days, year),
                               data,
                               family = poisson(link = "log"), 
                               method = 'REML')
summary(mSmoothDayYearInteract)
#Also R-sq.(adj) =  0.659   Deviance explained = 65.1%. BAD.

gam.check(mSmoothDayYearInteract, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayYearInteract, newdata = seasonalitiesData))
points(days, pred, col="blue")

#``````````````````````````````````````````````````````````
# With interactions. Method: Tensor product interactions (different smoothing bases)
#``````````````````````````````````````````````````````````
mSmoothDayYearTensorProduct <- gam(contacts~ti(days) + ti(month) + ti(days, month),
                                   bs = c("tp", "tp"), 
                                   data,
                                   family = poisson(link = "log"), 
                                   method = 'REML');
summary(mSmoothDayYearTensorProduct)
#Also R-sq.(adj) =  0.63   Deviance explained = 62.3%. BAD.

gam.check(mSmoothDayYearTensorProduct, rep=500)

plot(data$days,data$contacts, pch=19, cex=0.25)

points(days, true_mean, pch=3, col="red")

pred <- exp(predict.gam(mSmoothDayYearTensorProduct, newdata = seasonalitiesData))
points(days, pred, col="blue")


