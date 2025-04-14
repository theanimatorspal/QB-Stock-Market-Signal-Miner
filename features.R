source("scraper.R")

data <- get_stock_data()

generate_features <- function(data) {
  data_xts <- xts::xts(data$adjusted, order.by = data$date)
  return_xts <- dailyReturn(data_xts, type = "log")
  sma_10 <- SMA(data_xts, 10)
  sma_20 <- SMA(data_xts, 20)
  RSI_14 <- RSI(data_xts, 14) 
  macd <- MACD(data_xts) # elle xts nai return garxa
  macd_macd <- macd[, "macd"]
  macd_signal <- macd[, "signal"]
  bbands <- BBands(data_xts)
  bbands_down <- bbands$dn
  bbands_up <- bbands$up
  bbands_avg <- bbands$mavg
  data <- data %>% 
    mutate (
      returns = as.numeric(return_xts),
      sma_10 = as.numeric(sma_10),
      sma_20 = as.numeric(sma_20),
      RSI_14 = as.numeric(RSI_14),
      macd = as.numeric(macd_macd),
      signal = as.numeric(macd_signal),
      bbands_up = as.numeric(bbands_up),
      bbands_down = as.numeric(bbands_down),
      bbands_avg = as.numeric(bbands_avg)
    ) %>% 
    replace_na(list(sma_10 = 0,
                    sma_20 = 0,
                    RSI_14 = 0,
                    macd = 0,
                    signal = 0,
                    bbands_up = 0,
                    bbands_down = 0,
                    bbands_avg = 0
                    ))
  return (data)
}


features <- generate_features(data) 

macd_plot <- features %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = macd), colour = "blue", alpha = 0.1)+
  geom_line(aes(y = signal), colour = "red", alpha = 0.1)+
  geom_bar(aes(y = macd-signal,
               fill = macd - signal > 0),
           stat = "identity", # ggplot le feri count herna thaalxa, so plot identity
           alpha = 1)+
  scale_fill_manual(
    values = c("maroon", "skyblue"),
    labels = c("bear", "bull"),
    name = "MACD Histogram"
    )+
  coord_cartesian(xlim = as.Date(c("2023-1-1", "2024-2-1")))+
  theme_minimal()

bbands_plot <- features %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = bbands_up), color = "skyblue", linetype = "dashed") +
  geom_line(aes(y = bbands_down), color = "tomato", linetype = "dashed") +
  geom_line(aes(y = bbands_avg), color = "purple", linetype = "dotted") +
  labs(title = "📈 Bollinger Bands", y = "Price") +
  coord_cartesian(xlim = as.Date(c("2023-01-01", "2024-02-01")))+
  theme_minimal()

bbands_plot  / macd_plot
  
chart_series <- function () {
  stock_xts <- xts::xts(data[, c("open", "close", "adjusted", "high", "low")], order.by = data$date)
  chartSeries(stock_xts,
              type = "candlesticks",
              theme = chartTheme("white"),
              TA = c(addBBands(n = 20, sd = 2),
                     addRSI(n = 14)))
}
#labs(
#  title = "MACD indicator",
#  subtitle = "blue is macd, red is signal",
#  x = "Date",
#  y = "Value"
#) %>% 
  