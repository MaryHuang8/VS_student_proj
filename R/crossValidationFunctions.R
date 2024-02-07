library(mgcv)
library(tidyverse)
library(tibble)
library(dplyr)

generatePartitionsEvenData <- function(data, k) {
  s <- seq(1, nrow(data))
  b <- round(seq(1, nrow(data), length.out=k+1))
  c <- cut(s, breaks=b, labels = FALSE)
  return(split(s, c))
}

generatePartitionsEvenBinSize <- function(data, column, k) {
  min_val <- min(data[[column]])
  max_val <- max(data[[column]])
  
  s <- seq(1, nrow(data))
  b <- round(seq(min_val, max_val, length.out=k+1))
  c <- cut(data[[column]], breaks=b, labels=FALSE)
  return(split(s, c))
}

test <- function(model, data) {
  return(predict.gam(model, newdata=data, type="response") )
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

crossValidateGam <- function(data, partition, train, test, evaluate) {
  partitions <- partition(data)
  results_vector <- numeric(0)
  
  for (partition_ in partitions) {
    train_data <- data[-partition_, ]
    test__data <- data[ partition_, ]
    
    model <- train(train_data)
    # cv function "trusts" model doesn't use the column in its evaluation
    pred <- test(model, test__data)
    results_vector <- append(results_vector, evaluate(test__data, pred))
  }
  
  return(mean(results_vector))
}


# **************************************************************************** #

# Example use:

observationData <- getObservedData()

train <- function(data) {
  return (gam(contacts~s(days, k=100) + 
                lockdown + publicHoliday + schoolHoliday + weekdate, 
              data,
              family = poisson(link = "log"), 
              method = 'REML'
  ))
}

#crossValidateGam(data = observationData, 
#                 partition, 
#                 train, 
#                 test, 
#                 evaluate
#)