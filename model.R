source("features.R")

data <- get_stock_data()
features <- generate_features(data) %>% 
  mutate(
    direction = factor(ifelse(returns > 0, 1, 0))
  )

train_data <- features %>% 
  filter(date <= as.Date("2023-1-1"))
test_data <- features %>% 
  filter(date > as.Date("2023-1-1"))

model <- randomForest(direction ~ rsi_14 + macd + signal + bbands_avg,
                      data = train_data)

test_data %>% 
  mutate(
    predicted = predict(model, newdata = .)
  ) %>% 
  ggplot(aes(x = date, y = adjusted))+
    geom_line(color = "black", alpha = 0.1)+
    coord_cartesian(xlim = as.Date(c("2024-7-1", "2025-1-1")))+
    geom_point(aes(shape = predicted), size = 5)+
    geom_point(aes(color = direction), alpha = 0.5, size = 5)+
    scale_color_manual(values = c("red", "blue"))
  
 #   geom_point(aes(color = predicted), size = 2)+
 #   geom_point(aes(color = direction), size = 2)+
 #   scale_color_manual(values = c("red", "blue"), labels = c("up", "down"))

