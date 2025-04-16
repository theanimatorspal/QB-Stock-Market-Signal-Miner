source("base.R")

app <- function() {
  rmarkdown::run("dashboard.Rmd")
}

app()