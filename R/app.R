source("model.R")
source("portOpt.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("📈 Stock Signal Miner"),
  fluidRow(
    # Column 1: Inputs
    column(
      width = 3,
      wellPanel(
        dateRangeInput("date_range", "Select Date Range (Data Fetch)",
                       start = "2018-07-01", end = "2025-01-01"),
        dateRangeInput("date_range_view", "Select Date Range (View)",
                       start = "2022-07-01", end = "2025-01-01"),
        dateInput("date_upto", "Date Upto (For Training)", value = "2022-12-12"),
        selectInput("model_type", "Model Type", choices = c("rf", "xgb", "lbm")),
        textInput("retrain", "Retrain Model", value = FALSE),
        textInput("stock_symbol", "Stock Symbol", value = "AAPL"),
        selectInput("stock_exchange", "Stock Exchange",
                    choices = c("NOT_NEPSE", "NEPSE")),
        selectInput("feature_plot", "Select Feature Plot",
                    choices = c("Bollinger Bands" = "bbands",
                                "MACD" = "macd",
                                "RSI" = "rsi",
                                "Williams %R" = "willr",
                                "CCI" = "cci"),
                    selected = "bbands"),
        actionButton("run", "📊 Predict / Plot")
      )
    ),
    
    # Column 2: Tabset
    column(
      width = 6,
      tabsetPanel(
        tabPanel("💹 Predict", plotOutput("prediction_plot")),
        tabPanel("🧮 Features", plotOutput("features_plot")),
        tabPanel("📊 Data", tableOutput("data_table"))
      )
    ),
    
    # Column 3: Mining & Portfolio Analysis
    column(
      width = 3,
      wellPanel(
        dateInput("top_date", "📅 Analysis Date", value = Sys.Date()),
        actionButton("top_action", "⛏️ Mine"),
        hr(),
        fileInput("portfolio_file", "📂 Upload Portfolio CSV", accept = ".csv"),
        selectInput("portfolio_stock_exchange", "Portfolio Type",
                    choices = c("NEPSE" = "NEPSE", "not NEPSE" = "NOT_NEPSE"),
                    selected = "NOT_NEPSE"),
        actionButton("portfolio_analysis", "📈 Analyze Portfolio")
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$portfolio_analysis, {
    req(input$portfolio_file$datapath)
    se <- input$portfolio_stock_exchange
    file <- input$portfolio_file$datapath
    port <- ksai.portfolio.wrangle(file, type = se)
    opt_port <- ksai.portfolio.optimize(port, se)
    
    comparision <- full_join(
      port, opt_port,
      by = "symbol",
      suffix = c("_original", "_optimized")
    ) %>%
      mutate(across(c(value_original, value_optimized),
                    ~ round(replace_na(.x, 0), 4)))
    
    output$comparison <- renderTable(comparision)
    
    showModal(modalDialog(
      title = "📊 Portfolio Analysis",
      tableOutput("comparison"),
      easyClose = TRUE,
      size = "l",
      class = "dark-modal"
    ))
  })
  
  observeEvent(input$top_action, {
    req(input$stock_symbol)
    
    data <- ksai.get_stock_data(
      symbol = input$stock_symbol,
      start_date = input$date_range[1],
      end_date = input$top_date,
      stock_exchange = input$stock_exchange
    )
    
    features <- ksai.generate_features(data)
    
    stats <- features %>%
      select(-date) %>%
      summarise(across(everything(), list(
        mean = ~ round(mean(., na.rm = TRUE), 3),
        sd = ~ round(sd(., na.rm = TRUE), 3)
      )))
    
    stats_long <- stats %>%
      pivot_longer(everything(),
                   names_to = c("Feature", "Stat"),
                   names_sep = "_(?=[^_]+$)",
                   values_to = "Value") %>%
      pivot_wider(names_from = Stat, values_from = Value)
    
    rsi <- mean(features$rsi_14, na.rm = TRUE)
    macd <- mean(features$macd - features$signal, na.rm = TRUE)
    cci <- mean(features$cci_20, na.rm = TRUE)
    
    suggestions <- c()
    
    if (!is.na(rsi)) {
      if (rsi > 70) {
        suggestions <- c(suggestions, "📈 RSI high → Overbought → Sell/Hold")
      } else if (rsi < 30) {
        suggestions <- c(suggestions, "📉 RSI low → Oversold → Buy signal")
      } else {
        suggestions <- c(suggestions, "📊 RSI neutral → Monitor")
      }
    }
    
    if (!is.na(macd)) {
      if (macd > 0) {
        suggestions <- c(suggestions, "💹 MACD positive → Momentum up")
      } else if (macd < 0) {
        suggestions <- c(suggestions, "🔻 MACD negative → Momentum down")
      } else {
        suggestions <- c(suggestions, "⚖️ MACD neutral")
      }
    }
    
    if (!is.na(cci)) {
      if (cci > 100) {
        suggestions <- c(suggestions, "📈 CCI > 100 → Overbought")
      } else if (cci < -100) {
        suggestions <- c(suggestions, "📉 CCI < -100 → Oversold")
      } else {
        suggestions <- c(suggestions, "📊 CCI normal")
      }
    }
    
    stats_table <- paste0(
      "<table style='width:100%; border-collapse:collapse;'>",
      "<tr style='background:#f2f2f2;'><th>📌 Feature</th><th>Mean</th><th>SD</th></tr>",
      paste0(
        apply(stats_long, 1, function(row) {
          sprintf("<tr><td>%s</td><td>%.3f</td><td>%.3f</td></tr>",
                  row["Feature"], as.numeric(row["mean"]), as.numeric(row["sd"]))
        }),
        collapse = ""
      ),
      "</table>"
    )
    
    showModal(modalDialog(
      title = paste("📊 Stats for", input$stock_symbol),
      HTML(paste0(
        "<b>📅 Range:</b> ", input$date_range[1], " to ", input$top_date, "<br><br>",
        "<b>📉 Feature Summary:</b><br>", stats_table, "<br><br>",
        "<b>🧠 Signal Suggestion:</b><br>", paste(suggestions, collapse = "<br><br>")
      )),
      easyClose = TRUE,
      footer = modalButton("OK 👍"),
      class = "dark-modal"
    ))
  })
  
  run_event <- eventReactive(input$run, {
    date_start <- input$date_range[1]
    date_end <- input$date_range[2]
    date_start_view <- input$date_range_view[1]
    date_end_view <- input$date_range_view[2]
    
    data <- ksai.get_stock_data(
      symbol = input$stock_symbol,
      start_date = date_start,
      end_date = date_end,
      stock_exchange = input$stock_exchange)
    
    features <- ksai.generate_features(data) %>%
      mutate(direction = factor(ifelse(returns > 0, 1, 0)))
    
    train_data <- features %>% filter(date <= as.Date(input$date_upto))
    test_data <- features %>% filter(date > as.Date(input$date_upto))
    
    model_type <- input$model_type
    
    result <- ksai.model_predict(
      features,
      train_data,
      test_data,
      date_start = as.character(date_start_view),
      date_end = as.character(date_end_view),
      model_type = model_type,
      model_name = paste0(model_type, "-", input$stock_symbol),
      retrain = input$retrain == "TRUE"
    )
    result[[2]]
  })
  
  output$prediction_plot <- renderPlot({ run_event() })
  
  output$features_plot <- renderPlot({
    date_start <- input$date_range[1]
    date_end <- input$date_range[2]
    data <- ksai.get_stock_data(
      symbol = input$stock_symbol,
      start_date = date_start,
      end_date = date_end,
      stock_exchange = input$stock_exchange)
    features <- ksai.generate_features(data)
    plots <- ksai.plot_features(features,
                                start_date = input$date_range_view[1],
                                end_date = input$date_range_view[2])
    named_plots <- list(
      bbands = plots[[1]],
      macd   = plots[[2]],
      rsi    = plots[[3]],
      willr  = plots[[4]],
      cci    = plots[[5]]
    )
    named_plots[[input$feature_plot]]
  })
  
  output$data_table <- renderTable({
    date_start <- input$date_range[1]
    date_end <- input$date_range[2]
    data <- ksai.get_stock_data(
      symbol = input$stock_symbol,
      start_date = date_start,
      end_date = date_end,
      stock_exchange = input$stock_exchange)
    features <- ksai.generate_features(data)
    head(features, 10)
  })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
