pkgs <- c("tidyverse",
                "languageserver",
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
                "qs",
          
                "flexdashboard",
                "shiny",
          
                "reticulate",
          
                "PortfolioAnalytics",
                "readr",
                "ROI",
                "ROI.plugin.quadprog",
                "ROI.plugin.glpk"
          )

base <- function() {
  installed <- pkgs %in% rownames(installed.packages())
  
  if(any(!installed)) {
    install.packages(pkgs[!installed])
  }
  
  lapply(pkgs, library, character.only = TRUE)
  
}

base()
