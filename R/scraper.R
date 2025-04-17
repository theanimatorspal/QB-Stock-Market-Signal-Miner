source("base.R")

get_stock_data <- function(symbol = "AAPL",
                           start_date = "2018-01-01",
                           end_date = Sys.Date(), 
                           stock_exchange = "NOT_NEPSE"
                           ) {
  
  if (stock_exchange == "NOT_NEPSE") {
  tryCatch({
      data <- tq_get(symbol, from = start_date, to = end_date)
      if(nrow(data) == 0) {
       return (NULL) 
      }
      data <- data %>% 
        drop_na() %>% 
        arrange(date) %>% 
        select(-symbol)
      
      return(data)
    }, error = function (e) {
      message("Error f")
    })
  } else {
    py_require("pandas")
    py_require("selenium")
    py_require("webdriver_manager")
    py_run_file("../python/nepseScrapper.py") 
    
    scraper <- py$NepseScraper()
    scraper$browse(py$NepseScraper$Page$STOCK_TRADING)
    start_date <- format(as.Date(start_date), "%m/%d/%Y")
    end_date <- format(as.Date(end_date), "%m/%d/%Y")
    frame <- scraper$fetch_data_symbol(symbol, start_date, end_date)
    scraper$stop()
    
    frame <- data %>% arrange(Date) %>% 
      select(-SN) %>% 
      rename("date" = "Date",
             "volume" = "Total Traded Shares",
             "high" = "Max Price",
             "low" = "Min Price",
             "close" = "Close Price",
             "amount" = "Total Traded Amount",
             "transactions" = "Total Transactions"
             ) %>% 
      mutate (
        volume = str_replace(volume, ",", ""),
        amount = str_replace(amount, ",", ""),
        date = as.Date(date),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close),
        adjusted = as.numeric(close) # adjusted ko datai xaina
      ) %>% 
      select(date, high, low, close, adjusted)
  
    return (frame)
  }
}

scraper <- function() {
  stock_data <- get_stock_data()
}

scraper()