source("scraper.R")

#data <- get_stock_data()

generate_features <- function(data) {
  data_xts <- xts::xts(data, order.by = data$date)
  data_adjusted_xts <- xts::xts(data$adjusted, order.by = data$date)
  return_xts <- dailyReturn(data_adjusted_xts, type = "log")
  sma_10 <- SMA(data_adjusted_xts, 10)
  sma_20 <- SMA(data_adjusted_xts, 20)
  rsi_14 <- RSI(data_adjusted_xts, 14) 
  macd <- MACD(data_adjusted_xts) # elle xts nai return garxa
  momentum_10 <- momentum(data_adjusted_xts, n = 10)
  cci_20 <- CCI(data_adjusted_xts) # like quicker rsi
  willr_14 <- WPR(data_adjusted_xts, n = 14)
  macd_macd <- macd[, "macd"]
  macd_signal <- macd[, "signal"]
  bbands <- BBands(data_adjusted_xts)
  bbands_down <- bbands$dn
  bbands_up <- bbands$up
  bbands_avg <- bbands$mavg
  data <- data %>% 
    mutate (
      price = as.numeric(data_adjusted_xts),
      returns = as.numeric(return_xts),
      sma_10 = as.numeric(sma_10),
      sma_20 = as.numeric(sma_20),
      rsi_14 = as.numeric(rsi_14),
      macd = as.numeric(macd_macd),
      signal = as.numeric(macd_signal),
      bbands_up = as.numeric(bbands_up),
      bbands_down = as.numeric(bbands_down),
      bbands_avg = as.numeric(bbands_avg),
      willr_14 = as.numeric(willr_14),
      cci_20 = as.numeric(cci_20),
    ) %>% 
    replace_na(list(sma_10 = 0,
                    sma_20 = 0,
                    rsi_14 = 0,
                    macd = 0,
                    signal = 0,
                    bbands_up = 0,
                    bbands_down = 0,
                    bbands_avg = 0,
                    willr_14 = 0,
                    cci_20 = 0
                    ))
  return (data)
}

#features <- generate_features(data) 


plot_features <- function (features, start_date = "2023-1-1", end_date = "2024-2-1") {
  date_limit <- as.Date(c(start_date, end_date))
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
    coord_cartesian(xlim = date_limit)+
    theme_minimal()
  
  bbands_plot_yrange <- range(features$bbands_up) + c(-10, 10)
  bbands_plot <- features %>% 
    ggplot(aes(x = date))+
    #geom_line(aes(y = bbands_up), color = "skyblue", linetype = "dashed") +
    #geom_line(aes(y = bbands_down), color = "tomato", linetype = "dashed") +
    #geom_line(aes(y = bbands_avg), color = "purple", linetype = "dotted") +
    labs(title = "📈 Bollinger Bands", y = "Price") +
    geom_bbands(aes(high = high, low = low, close = close))+
    coord_cartesian(xlim = date_limit, ylim = range(features$bbands_up,
                                                    features$bbands_down) + c(-10, 10))+
    theme_minimal()
  
  plot_line <- function(inY, inName) {
    return(
      features %>%
        ggplot(aes(x = date)) +
        geom_line(aes(y = inY))+
        labs(title = inName)+
        coord_cartesian(xlim = date_limit)+
        theme_minimal()
    )
  }
  
  rsi_plot <- plot_line(features$rsi_14, "RSI")
  willr_14_plot <- plot_line(features$willr_14, "Willr_14")
  cci_plot <- plot_line(features$cci_20, "cci_20")
  
  return(list(bbands_plot, macd_plot, rsi_plot, willr_14_plot, cci_plot))
}

#plot <- plot_features(features, "2022-4-1", "2022-5-5")