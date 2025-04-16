source("base.R")

get_stock_data <- function(symbol = "AAPL",
                           start_date = "2018-01-01",
                           end_date = Sys.Date()) {
  tryCatch({
    data <- tq_get(symbol, from = start_date, to = end_date)
    if(nrow(data) == 0) {
     return (NULL) 
    }
    data <- data %>% 
      drop_na() %>% 
      arrange(date)
    return(data)
  }, error = function (e) {
    message("Error f")
    
  })
}

scraper <- function() {
  stock_data <- get_stock_data()
}

scraper()
