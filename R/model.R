source("features.R")

ksai.model_predict <- function(features,
                          train_data,
                          test_data,
                          date_start = "2024-7-1",
                          date_end = "2025-1-1",
                          model_type = "rf",
                          model_name = "model",
                          retrain = FALSE) {
  
  model_dir <- "../models/"
  if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
  
  model_name <- paste0("model_", model_type, model_name, ".rds")
  model_path <- file.path(model_dir, model_name)
  
  model <- NULL
  test_plot <- NULL
  predicted_ <- NULL
  
  train_data_as_matrix <- as.matrix(train_data[, c("rsi_14", "macd", "signal", "bbands_avg")])
  test_data_as_matrix <- as.matrix(test_data[, c("rsi_14", "macd", "signal", "bbands_avg")])
  
  if (file.exists(model_path) && !retrain) {
    message("Loading existing model: ", model_path)
    model <- readRDS(model_path)
  } else {
    message("Training (or retraining) model: ", model_type)
    
    if (model_type == "rf") {
      model <- randomForest(direction ~ rsi_14 + macd + signal + bbands_avg,
                            data = train_data)
    } else if (model_type == "xgb") {
      label <- as.numeric(as.character(train_data$direction))
      xgb_dtrain <- xgb.DMatrix(data = train_data_as_matrix, label = label)
      model <- xgboost(data = xgb_dtrain, nrounds = 50,
                       objective = "binary:logistic", verbose = 0)
    } else if (model_type == "lbm") {
      label <- as.numeric(as.character(train_data$direction))
      lgb_dtrain <- lgb.Dataset(data = train_data_as_matrix, label = label)
      model <- lgb.train(params = list(objective = "binary", metric = "binary_logloss"),
                         data = lgb_dtrain, nround = 50)
    } else {
      stop("Unsupported model type!")
    }
    
    saveRDS(model, model_path)
    message("Model saved to: ", model_path)
  }
  
  if (model_type == "rf") {
    predicted_ <- predict(model, newdata = test_data)
    test_plot <- test_data %>% 
      mutate(predicted = predicted_) %>%
      ggplot(aes(x = date, y = adjusted)) +
      geom_line(color = "black", alpha = 0.1) +
      coord_cartesian(xlim = as.Date(c(date_start, date_end))) +
      geom_ribbon(aes(ymin = adjusted - 5, ymax = adjusted + 5,
                      fill = predicted == direction), alpha = 0.5)
  } else {
    predicted_ <- predict(model, newdata = test_data_as_matrix)
    test_plot <- test_data %>% 
      mutate(predicted = predicted_,
             predicted_classes = cut(as.numeric(predicted_),
                                     breaks = c(0, 0.33, 0.66, 1),
                                     labels = c("low", "med", "high")),
             correct = ifelse(predicted > 0.5, 1, 0) == direction,
             difference = as.numeric(direction) - as.numeric(predicted_)) %>%
      ggplot(aes(x = date, y = adjusted)) +
      geom_line(color = "black", alpha = 0.1) +
      coord_cartesian(xlim = as.Date(c(date_start, date_end))) +
      geom_ribbon(aes(ymin = adjusted - predicted * 10,
                      ymax = adjusted + predicted * 10,
                      fill = correct), alpha = 0.5) +
      scale_color_manual(values = c("red", "blue"))
  }
  
  return(list(model = model, plot = test_plot))
}

sample <- function() {

  data <- ksai.get_stock_data()
  features <- ksai.generate_features(data) %>% 
    mutate(
      direction = factor(ifelse(returns > 0, 1, 0))
    )
  
  train_data <- features %>% 
    filter(date <= as.Date("2023-1-1"))
  test_data <- features %>% 
    filter(date > as.Date("2023-1-1"))
  
  prediction_rf <- ksai.model_predict(features,
                                 train_data,
                                 test_data,
                                 model_type = "rf")
  
  
  prediction_xgb <- ksai.model_predict(features,
                                 train_data,
                                 test_data,
                                 model_type = "xgb")
  
  prediction_lbm <- ksai.model_predict(features,
                                 train_data,
                                 test_data,
                                 model_type = "lbm")
  
  (prediction_rf[[2]] + ggtitle("Random Forest")) /
    (prediction_xgb[[2]] + ggtitle("XGBoost")) /
    (prediction_lbm[[2]] + ggtitle("LightGBM"))
#}
}