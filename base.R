pkgs <- c("tidyverse",
                "tidyquant",
                "quantmod",
                "TTR",
                "dplyr",
                "tidyr",
                "lubridate",
                "stringr")

base <- function() {
  installed <- pkgs %in% rownames(installed.packages())
  
  if(any(!installed)) {
    install.packages(pkgs[!installed])
  }
  
  lapply(pkgs, library, character.only = TRUE)
  
}

base()
