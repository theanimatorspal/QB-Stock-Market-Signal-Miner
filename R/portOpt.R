source("features.R")

ksai.portfolio.wrangle <- function(csv_file, type = "NEPSE") {
  df <- read_csv(csv_file)
  if (type == "NEPSE") {
    wrangled <- df %>% 
      select(-1, -3, -4, -6, -7) %>% 
      head(-1) %>% 
      rename("symbol" = 1,
             "value" = 2)  %>% 
      mutate (value = value / sum(value))
    return (wrangled)
  }
  else {
    return (df)  
  }
}

ksai.portfolio.optimize <- function (portfolio_wrangled, stock_exchange = "NEPSE") {
  portfolio_prices <- portfolio_wrangled$symbol %>% 
    set_names() %>% 
    map(~ ksai.get_stock_data(symbol = .x, stock_exchange = stock_exchange
                              )$adjusted) %>% 
    bind_cols()
  
  portfolio_returns <- ROC(portfolio_prices, type = "discrete") %>% na.omit()
  portfolio_spec <- portfolio.spec(colnames(portfolio_returns))
  portfolio_spec <- add.constraint(portfolio_spec,
                                   type = "weight_sum",
                                   min_sum = 1,
                                   max_sum = 1)
  portfolio_spec <- add.constraint(portfolio_spec,
                                   type = "box",
                                   min = 0.1,
                                   max = 0.4)
  portfolio_spec <- add.objective(portfolio_spec,
                                   type = "return",
                                   name = "mean")
  portfolio_spec <- add.objective(portfolio_spec,
                                   type = "risk",
                                   name = "StdDev")
  optimized_port <- optimize.portfolio(portfolio_returns,
                                       portfolio_spec,
                                       optimize_method = "ROI",
                                       trace = TRUE) 
  return (optimized_port)
}
