library(mgcv)
library(tidyverse)
library(expss)
library(gridExtra) # coplot

source("simulator2.R")
source("crossValidationFunctions.R")
# Plotting function 1 ``````````````````````````````````````````

# input list models, graph title, number of graphs by column and per row.
plotModelPredictions <- function(models, title, height, width) {
  par(mfrow = c(height, width))
  for (model in models) {
    plot(data$days, data$contacts, pch=19, cex=0.25,
         main = attr(model.matrix(model), "dimnames")[[2]][2], 
         xlab = "Days", ylab = "Number of Contacts")
    
    true_mean <- getTrueMean__()
    points(days, true_mean, pch=3, col="red")
    
    pred <- exp(predict.gam(model, newdata = seasonalitiesData))
    points(days, pred, col="blue")
  }
  mtext(title, side = 3, line = - 1, outer = TRUE)
}

# Plotting function 2 `````````````````````````````````````````

# input list of models, each model title, graph title, number of graphs by column and per row.
ggPlotModelPredictions <- function(models, model_names, title, height, width) {
  par(mfrow = c(height,width))
  true_mean <- getTrueMean__()
  i <- 1
  output_graphs <- list()
  
  for (model in models) {
    pred <- exp(predict.gam(model, newdata = seasonalitiesData))
    
    graph <- ggplot() + 
      geom_point(data = data.frame(data$days, data$contacts), 
                 aes(x = data$days, y = data$contacts), 
                 color = "black", size = 1) + 
      geom_point(data = data.frame(days, true_mean), 
                 aes(x = days, y = true_mean), shape = 3, color="red") +
      geom_point(data = data.frame(days, pred), 
                 aes(x = days, y = pred), color = 'blue') +
      labs(title=model_names[i], x="", y="")
    
    output_graphs[[i]] <- ggplotGrob(graph)
    i <- i + 1
  }
  
  grid.arrange(grobs = output_graphs, ncol = width, nrow = height, 
               name = title, top = title, 
               left = "Number of Contact", bottom = "Days"
  )
}

# Plotting function 3 `````````````````````````````````````````

# input list of models, each model title, each's resulting performance metric, 
# graph title, number of graphs by column and per row.
ggPlotModelPredictionsRMSE <- function(models, model_names, RMSEs, title, height, width) {
  par(mfrow = c(height,width))
  true_mean <- getTrueMean__()
  i <- 1
  output_graphs <- list()
  
  for (model in models) {
    pred <- exp(predict.gam(model, newdata = seasonalitiesData))
    
    graph <- ggplot() + 
      geom_point(data = data.frame(data$days, data$contacts), 
                 aes(x = data$days, y = data$contacts), 
                 color = "black", size = 1) + 
      geom_point(data = data.frame(days, true_mean), 
                 aes(x = days, y = true_mean), shape = 3, color="red") +
      geom_point(data = data.frame(days, pred), 
                 aes(x = days, y = pred), color = 'blue') +
      labs(title=model_names[i], 
           x=paste("RMSE:", 
                   format(round(RMSEs[i], 2), nsmall = 2), 
                   "Dev expl:", 
                   format(round(summary(model)$dev.expl, 2), nsmall = 2)), 
           y="")
    
    output_graphs[[i]] <- ggplotGrob(graph)
    i <- i + 1
  }
  
  grid.arrange(grobs = output_graphs, ncol = width, nrow = height, 
               name = title, top = title, 
               left = "Number of Contact", bottom = "Days"
  )
}

# plotting function 4 ``````````````````````````````````````````

# helper function, producing graph after obtaining cross validation results
# input data collected, list of models, each model title, 
# each model's predictions, each's resulting performance metric,
# graph title, y label, x label, number of graphs by column and per row.

ggPlotModelPredictionsCrossVldn <- function(data, models, model_names, preds, 
                                            RMSEs, title, ylabel, xlabel, 
                                            height, width) {
  par(mfrow = c(height,width))
  true_mean <- getTrueMean__()
  i <- 1
  output_graphs <- list()
  
  dataLength <- length(data$days)
  numPartitions <- 5
  partitionIndices <- round(seq(1, dataLength, length.out=numPartitions+1))
  # Assumption of pre-sorted data here to ensure even partitions of data
  partitions <- data$days[partitionIndices]
  
  for (model in models) {
    #partition color block
    left <- partitions[i]
    rght <- partitions[i+1]
    # print(paste("left", left, "right", rght))
    # print(paste("length days[left:rght]", length(days[left:rght]), 
    #             "length preds[i][left:rght]", length(preds[i][left:rght])))
    # print(paste("length days", length(days), 
    #             "length preds[i]", length(preds[i])))
    
    #pred <- exp(predict.gam(model, newdata = data_partitions[i]))
    #pred <- exp(predict.gam(model, newdata = data))
    print(preds[i]$pred)
    #pred_pts_df <- data.frame(data$days[data$days >= left & data$days <= rght], preds[i])
    #pred_pts_df <- data.frame(days[left:rght], preds[i])
    # colnames(pred_pts_df)[1] <- "day"
    # colnames(pred_pts_df)[2] <- "contact"
    # print(colnames(pred_pts_df))
    # print(pred_pts_df)
    
    graph <- ggplot() + 
      geom_rect(aes(xmin=left, xmax=rght, ymin=0, ymax=Inf), alpha=0.5, fill="lightblue") +
      geom_point(data = data.frame(data$days, data$contacts), 
                 aes(x = data$days, y = data$contacts), 
                 color = "black", size = 1) + 
      geom_point(data = data.frame(days, true_mean), 
                 aes(x = days, y = true_mean), shape = 3, color="red") +
      geom_point(data = data.frame(preds[i]), 
                 aes(x = day, y = pred), color = 'blue') +
      labs(title=model_names[i], 
           x=paste("RMSE:", 
                   format(round(RMSEs[i], 2), digits=3), 
                   "Dev expl:", 
                   format(round(summary(model)$dev.expl, 2), nsmall = 2)),
           y="")
    for (partition in partitions) {
      graph <- graph + geom_vline(xintercept=partition,linetype=3)
    }
    
    output_graphs[[i]] <- ggplotGrob(graph)
    i <- i + 1
  }
  
  grid.arrange(grobs = output_graphs, ncol = width, nrow = height, 
              top = title, 
              left = ylabel, 
              bottom = xlabel, 
              right = paste("RMSE:", format(round(mean(RMSEs), 2), nsmall = 2))
              )
  #return (output_graphs)
}

# Plotting function 5 ``````````````````````````````````

# Cross validate and produce graph for each iteration of the cross validation process

crossValidateGamPlot <- function(data, partition, train, test, evaluate,
                                 title, ylabel, xlabel) {
  partitions <- partition(data)
  results_vector <- numeric(0)
  models <- list()
  preds <- list()
  i <- 1
  for (partition_ in partitions) {
    train_data <- data[-partition_, ]
    test__data <- data[ partition_, ]
    model <- train(train_data)
    models[[i]] <- model
    
    # cv function "trusts" model doesn't use the column in its evaluation
    pred <- test(model, test__data)
    test__data$pred <- pred
    print("test data-----------------")
    print(test__data, n=1000)
    print("--------------------------")
    agg_pred_df <- aggregate(data.frame(test__data$days, test__data$pred), 
                             by = list(test__data$days), FUN = mean)
    
    colnames(agg_pred_df)[2] <- "day"
    colnames(agg_pred_df)[3] <- "pred"
    print(agg_pred_df)
   
    test_RMSE <- evaluate(test__data, pred)
    results_vector <- append(results_vector, test_RMSE)
    
    print("results vector -----------")
    print(results_vector)
    print("--------------------------")
    
    preds[[i]] <- agg_pred_df
    
    i <- i + 1
  }
  
  ggPlotModelPredictionsCrossVldn(data, models, 1:length(partitions), preds,
                                  results_vector, title, ylabel, xlabel,
                                  1, length(partitions))
  #return(ggplots)
}

# Plotting function 6 ``````````````````````````````````

# plot number of knots VS RMSE graph for smoothing term knots tuning

knotsTuningPlot <- function(data, partition, changeKnotsTrain, test, evaluate,
                            nKnotsLowerBound, nKnotsUpperBound) {
  knots_tune_RMSE_list <- list(NULL)
  for (knot in nKnotsLowerBound:nKnotsUpperBound) {
    train <- function(data) {
      return (changeKnotsTrain(data, knot))
    }
    knots_tune_RMSE_list[[knot]] <- crossValidateGam(data, partition, train, test, evaluate)
    print(paste(knot, knots_tune_RMSE_list[[knot]]))
  }
  nKnots <- nKnotsLowerBound:nKnotsUpperBound
  knots_RMSE_df <- data.frame(unlist(nKnots), unlist(knots_tune_RMSE_list))
  colnames(knots_RMSE_df)[1] <- "nKnots"
  colnames(knots_RMSE_df)[2] <-  "RMSE"
  knots_RMSE_df$lab <- paste("k =", knots_RMSE_df$nKnots, ", \nRMSE:", format(round(knots_RMSE_df$RMSE, 2), digits = 3, format = "f"))
  model_knots_tune_plot <- ggplot(data = knots_RMSE_df, aes(x = nKnots, y = log(RMSE))) +
    geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k=30), color="darkgrey") + 
    geom_line(color="grey") +
    geom_point(color="black", size=2) +
    geom_point(data = knots_RMSE_df[which.min(knots_RMSE_df$RMSE), ], color="blue", size=4) +
    geom_text(data = knots_RMSE_df[which.min(knots_RMSE_df$RMSE), ], aes(nKnots, log(RMSE)+1, label=lab),color="blue") +
    labs(title="Smoothing Term Tuning Without Mechanistic Predictors: Knots VS Cross Validated RMSE", 
         x="Number of Knots", 
         y="ln(RMSE)")
  return(model_knots_tune_plot)
}

# examples of function changeKnotsTrain:

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