source("features.R")

model_predict <- function(features,
                                  train_data,
                                  test_data,
                                  date_start = "2024-7-1",
                                  date_end = "2025-1-1",
                                  model_type = "rf"
                                  ) {
  model <- NULL
  test_plot <- NULL
  predicted <- NULL
  test_plot <- NULL
  train_data_as_matrix <- as.matrix(train_data[, c("rsi_14",
                                                     "macd",
                                                     "signal",
                                                     "bbands_avg")])
  test_data_as_matrix <- as.matrix(test_data[, c("rsi_14",
                                                     "macd",
                                                     "signal",
                                                     "bbands_avg")])
  if (model_type == "rf") {
    model <- randomForest(direction ~ rsi_14 + macd + signal + bbands_avg,
                          data = train_data)
    predicted_ <- predict(model, newdata = test_data)
    test_plot <- test_data %>% 
      mutate(
        predicted = predicted_
      ) %>% 
      ggplot(aes(x = date, y = adjusted))+
        geom_line(color = "black", alpha = 0.1)+
        coord_cartesian(xlim = as.Date(c(date_start, date_end)))+
        geom_ribbon(aes(ymin = adjusted - 5, ymax = adjusted + 5, fill = predicted == direction), alpha = 0.3)
  } else if (model_type == "xgb") {
    label <- as.numeric(as.character(train_data$direction))
    xgb_dtrain <- xgb.DMatrix(data = train_data_as_matrix, label = label)
    xgb_dtest <- xgb.DMatrix(data = test_data_as_matrix )
    model <- xgboost(data = xgb_dtrain, nrounds = 50,
                    objective = "binary:logistic", verbose = 0) 
    predicted_ <- predict(model, xgb_dtest)
    test_plot <- test_data %>% 
      mutate(
        predicted = cut(as.numeric(predicted_),
                        breaks = c(0, 0.33, 0.66, 1),
                        labels = c( "low", "med", "high")),
        difference = as.numeric(direction) - as.numeric(predicted_)
      ) %>% 
      ggplot(aes(x = date, y = adjusted))+
        geom_line(color = "black", alpha = 0.1)+
        geom_ribbon(aes(ymin = adjusted - 5,
                        ymax = adjusted + 5,
                        fill = abs(difference) < 0.3),
                        alpha = 0.3)+
        coord_cartesian(xlim = as.Date(c(date_start, date_end)))+
        scale_color_manual(values = c("red", "blue"))
  } else if (model_type == "lbm") {
    label <- as.numeric(as.character(train_data$direction))
    lgb_dtrain <- lgb.Dataset(data = train_data_as_matrix, label = label)
    model <- lgb.train(params = list(objective = "binary",
                                     metric = "binary_logloss"),
                       data = lgb_dtrain, 
                       nround = 50)
    predicted_ <- predict(model, test_data_as_matrix)
    test_plot <- test_data %>% 
        mutate(
          predicted = cut(as.numeric(predicted_),
                          breaks = c(0, 0.33, 0.66, 1),
                          labels = c( "low", "med", "high")),
        difference = as.numeric(direction) - as.numeric(predicted_)
        ) %>% 
        ggplot(aes(x = date, y = adjusted))+
          geom_line(color = "black", alpha = 0.1)+
          coord_cartesian(xlim = as.Date(c(date_start, date_end)))+
          geom_ribbon(aes(ymin = adjusted - 5,
                          ymax = adjusted + 5,
                          fill = abs(difference) < 0.3),
                          alpha = 0.3)+
          scale_color_manual(values = c("red", "blue"))
      
  }
  
  return (list(model, test_plot))   
} 

data <- get_stock_data()
features <- generate_features(data) %>% 
  mutate(
    direction = factor(ifelse(returns > 0, 1, 0))
  )

train_data <- features %>% 
  filter(date <= as.Date("2023-1-1"))
test_data <- features %>% 
  filter(date > as.Date("2023-1-1"))

prediction_rf <- model_predict(features,
                               train_data,
                               test_data,
                               model_type = "rf")


prediction_xgb <- model_predict(features,
                               train_data,
                               test_data,
                               model_type = "xgb")

prediction_lbm <- model_predict(features,
                               train_data,
                               test_data,
                               model_type = "lbm")

prediction_rf[2] / prediction_xgb[2] / prediction_lbm[2]