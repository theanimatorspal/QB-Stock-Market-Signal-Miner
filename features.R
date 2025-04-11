source("scraper.R")

generate_features <- function(data) {
  data_xts <- xts::xts(data$adjusted, order.by = data$date)
  return_xts <- dailyReturn(data_xts, type = "log")
  sma_10 <- SMA(data_xts, 10)
  sma_20 <- SMA(data_xts, 10)
  data <- data %>% 
    mutate (
      returns = as.numeric(return_xts),
      sma_10 = as.numeric(sma_10),
      sma_20 = as.numeric(sma_20)
    ) %>% 
    replace_na(list(sma_10 = 0, sma_20 = 0))
  return (data)
}

stock_data <- get_stock_data()
features <- generate_features(stock_data)

visualizations <- function() {
  
  ggplot(data = features) +
    geom_point(mapping =
                 aes(x = date,
                     y = sma_10,
                     color = sma_20,
                     alpha = returns)) +
    scale_color_gradient(low = "#ff0000", high = "#000000")
  
  
  ggplot(data = features) +
    geom_point(mapping =
                 aes(x = date,
                     y = sma_10,
                     color = sma_20 < 100,
                     alpha = returns))
  ##  scale_color_gradient(low = "#ff0000", high = "#000000")
  
  
  ggplot(data = features) +
    geom_point(mapping =
                 aes(x = date,
                     y = sma_10,
                     color = sma_20,
                     alpha = returns))+
    facet_wrap(~ sma_20 > 100, nrow = 2)
  
  
  ggplot(data = features) +
    geom_point(mapping =
                 aes(x = date,
                     y = sma_10,
                     color = sma_20,
                     alpha = returns))+
    facet_wrap(returns > 0 ~ sma_20 > 100)
  
  ggplot(data = features) +
    geom_point(mapping =
                 aes(x = date,
                     y = sma_10,
                     color = sma_20,
                     alpha = returns))+
    facet_grid(. ~ sma_20 > 100)
  
  ggplot(data = features) + 
    geom_col(mapping = aes(x = date, y = sma_20, fill = returns), show.legend = FALSE)+
    geom_point(mapping =
                 aes(x = date,
                     y = sma_10,
                     color = returns > 0,
                     alpha = returns))+
    geom_smooth(data = features %>% filter(sma_20 < 10), mapping = aes(x = date, y = sma_10))+
    scale_fill_gradient(low = "red", high = "black")
}

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = after_stat(prop), group = 1))

ggplot(data = diamonds)+
  stat_summary(mapping =  aes(x = cut, y = depth),
               fun.min = min,
               fun.max = max,
               fun = median) # paxadi ko sap functions ho
