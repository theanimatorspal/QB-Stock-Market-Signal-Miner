pkgs <- c("tidyverse",
                "tidyquant",
                "quantmod",
                "TTR",
                "dplyr",
                "tidyr",
                "lubridate",
                "stringr",
                "plotly",
                "patchwork",
                "randomForest",
                "xgboost",
                "lightgbm",
          
                "flexdashboard",
                "shiny",
          
                "reticulate"
          )

base <- function() {
  installed <- pkgs %in% rownames(installed.packages())
  
  if(any(!installed)) {
    install.packages(pkgs[!installed])
  }
  
  lapply(pkgs, library, character.only = TRUE)
  
}

base()
