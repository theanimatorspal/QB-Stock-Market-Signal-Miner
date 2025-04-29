source("base.R")

ksai.get_stock_data <- function(symbol = "AAPL",
                                start_date = "2018-01-01",
                                end_date = Sys.Date(), 
                                stock_exchange = "NOT_NEPSE",
                                cache_dir = "../data/") {
  
  if (!dir.exists(cache_dir)) dir.create(cache_dir)
  
  cache_file <- file.path(
    cache_dir,
    paste0(stock_exchange, "_", symbol, "_", start_date, "_", end_date, ".qs")
  )
  
  if (file.exists(cache_file)) {
    return(qs::qread(cache_file))
  }
  
  if (stock_exchange == "NOT_NEPSE") {
    tryCatch({
      data <- tq_get(symbol, from = start_date, to = end_date)
      if (nrow(data) == 0) return(NULL)
      
      data <- data %>%
        drop_na() %>%
        arrange(date) %>%
        select(-symbol)
      
      qs::qsave(data, cache_file)
      return(data)
    }, error = function(e) {
      message(e)
      return(NULL)
    })
  } else {
    py_require("pandas")
    py_require("selenium")
    py_require("webdriver_manager")
    py_run_file("../python/nepseScrapper.py") 
    
    scraper <- py$NepseScraper()
    scraper$browse(py$NepseScraper$Page$STOCK_TRADING)
    start_date_fmt <- format(as.Date(start_date), "%m/%d/%Y")
    end_date_fmt <- format(as.Date(end_date), "%m/%d/%Y")
    frame <- scraper$fetch_data_symbol(symbol, start_date_fmt, end_date_fmt)
    scraper$stop()
    
    data <- frame %>% arrange(Date) %>% 
      select(-SN) %>% 
      rename(
        date = "Date",
        volume = "Total Traded Shares",
        high = "Max Price",
        low = "Min Price",
        close = "Close Price",
        amount = "Total Traded Amount",
        transactions = "Total Transactions"
      ) %>% 
      mutate(
        volume = as.numeric(gsub(",", "", volume)),
        amount = as.numeric(gsub(",", "", amount)),
        date = as.Date(date),
        high = as.numeric(high),
        low = as.numeric(low),
        close = as.numeric(close),
        adjusted = close
      ) %>%
      select(date, high, low, close, adjusted)
    
    qs::qsave(data, cache_file)
    return(data)
  }
}
