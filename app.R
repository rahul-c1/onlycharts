library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(TTR)
options(shiny.maxRequestSize=1000*1024^2)

#The 6 SOS Conditions Tracked:

#Range Expansion ≥1.5x (today's range is 50%+ larger than 10-day average)
#Volume Surge ≥1.2x (today's volume is 20%+ above 10-day average)
#Price Increase >2% (bullish momentum)
#Above 10-day SMA (short-term uptrend)
#Above 20-day SMA (intermediate uptrend)
#Not Extended ≤2 consecutive up days (not too late to enter)

# UI
ui <- fluidPage(
  titlePanel("Stock Chart Analysis with Technical Indicators"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      fileInput("datafile", "Upload CSV (symbol, date, open, high, low, close, volume)",
                accept = c(".csv")),
      
      hr(),
      textInput("symbol", "Enter Symbol:", value = ""),
      
      numericInput("lookback_days", "Lookback Period (days):",
                   value = 60, min = 20, max = 252, step = 10),
      
      actionButton("show_chart", "Show Chart", class = "btn-primary btn-block"),
      
      hr(),
      h4("Chart Settings"),
      checkboxInput("show_sma", "Show 10-day SMA", value = TRUE),
      checkboxInput("show_sma20", "Show 20-day SMA", value = TRUE),
      
      hr(),
      helpText("Conditions Score tracks 6 SOS criteria: Range Expansion (≥1.5x), Volume Surge (≥1.2x), Price Up (>2%), Above 10-SMA, Above 20-SMA, Not Extended (≤2 days up).")
    ),
    
    mainPanel(
      plotOutput("price_chart", height = "400px"),
      plotOutput("volume_chart", height = "200px"),
      plotOutput("conditions_chart", height = "200px"),
      br(),
      verbatimTextOutput("stock_analysis")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data storage
  stock_data <- reactiveVal(NULL)
  
  # Load data
  observeEvent(input$datafile, {
    req(input$datafile)
    
    tryCatch({
      df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
      
      # Validate columns
      required_cols <- c("symbol", "date", "open", "high", "low", "close", "volume")
      if (!all(required_cols %in% tolower(names(df)))) {
        showNotification("CSV must have columns: symbol, date, open, high, low, close, volume",
                        type = "error", duration = 5)
        return(NULL)
      }
      
      # Standardize column names
      names(df) <- tolower(names(df))
      df$date <- as.Date(df$date)
      
      stock_data(df)
      showNotification("Data loaded successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 5)
    })
  })
  
  # Calculate technical indicators
  calculate_indicators <- function(df) {
    df <- df %>%
      arrange(date) %>%
      mutate(
        # Simple Moving Averages
        sma_10 = SMA(close, n = 10),
        sma_20 = SMA(close, n = 20),
        
        # Range calculations
        range = high - low,
        avg_range_10 = zoo::rollmean(range, k = 10, fill = NA, align = "right"),
        range_expansion = range / lag(avg_range_10, 1),
        
        # Price change
        pct_change = (close - lag(close, 1)) / lag(close, 1) * 100,
        
        # Volume
        avg_volume_10 = zoo::rollmean(volume, k = 10, fill = NA, align = "right"),
        volume_surge = volume / avg_volume_10,
        
        # Running metrics
        days_up = ifelse(close > lag(close, 1), 1, 0),
        consecutive_up = NA,
        
        # Price levels
        high_20 = zoo::rollapply(high, width = 20, FUN = max, fill = NA, align = "right", partial = TRUE),
        low_20 = zoo::rollapply(low, width = 20, FUN = min, fill = NA, align = "right", partial = TRUE),
        gain_from_20d_low = (close - low_20) / low_20 * 100
      )
    
    # Calculate consecutive up days
    df <- df %>%
      mutate(
        consecutive_up = ave(days_up, cumsum(days_up == 0), FUN = cumsum)
      )
    
    # Calculate Stochastic (8,3,3)
    # nFastK = lookback period, nFastD = smoothing for %K, nSlowD = smoothing for %D
    stoch_result <- stoch(HLC = df[, c("high", "low", "close")], 
                          nFastK = 8, 
                          nFastD = 3, 
                          nSlowD = 3,
                          bounded = TRUE,
                          smooth = 1)
    
    # The result has fastK, fastD, slowD columns
    # For (8,3,3): fastK is raw stochastic, fastD is %K (smoothed), slowD is %D
    df$stoch_k <- stoch_result[, "fastD"]  # This is %K (8-period stochastic, 3-period smoothed)
    df$stoch_d <- stoch_result[, "slowD"]  # This is %D (3-period smoothing of %K)
    
    # Calculate setup conditions score
    df <- df %>%
      mutate(
        # Define conditions (1 if met, 0 if not)
        cond_range_expansion = ifelse(!is.na(range_expansion) & range_expansion >= 1.5, 1, 0),
        cond_volume_surge = ifelse(!is.na(volume_surge) & volume_surge >= 1.2, 1, 0),
        cond_price_increase = ifelse(!is.na(pct_change) & pct_change > 2, 1, 0),
        cond_above_sma10 = ifelse(!is.na(sma_10) & close > sma_10, 1, 0),
        cond_above_sma20 = ifelse(!is.na(sma_20) & close > sma_20, 1, 0),
        cond_not_extended = ifelse(!is.na(consecutive_up) & consecutive_up <= 2, 1, 0),
        
        # Sum all conditions
        conditions_met = cond_range_expansion + cond_volume_surge + cond_price_increase + 
                        cond_above_sma10 + cond_above_sma20 + cond_not_extended,
        
        # Color coding for bar chart
        condition_strength = case_when(
          conditions_met >= 5 ~ "Strong",
          conditions_met >= 3 ~ "Moderate",
          TRUE ~ "Weak"
        )
      )
    
    return(df)
  }
  
  # Show chart when button is clicked
  observeEvent(input$show_chart, {
    req(input$symbol, stock_data())
    
    symbol <- toupper(trimws(input$symbol))
    
    df <- stock_data() %>%
      filter(symbol == !!symbol) %>%
      arrange(date) %>%
      tail(input$lookback_days)
    
    if (nrow(df) == 0) {
      showNotification("Symbol not found in data", type = "error")
      return(NULL)
    }
    
    # Calculate indicators
    df_calc <- calculate_indicators(df)
    
    # Price Chart
    output$price_chart <- renderPlot({
      p <- ggplot(df_calc, aes(x = date)) +
        # Candlesticks (high-low lines)
        geom_segment(aes(xend = date, y = low, yend = high), color = "gray40", size = 0.5) +
        # Candlestick bodies
        geom_segment(aes(xend = date, y = open, yend = close, color = close >= open), size = 2) +
        scale_color_manual(values = c("TRUE" = "#00CC66", "FALSE" = "#FF3333"), guide = "none")
      
      # Add moving averages if selected
      if (input$show_sma) {
        p <- p + geom_line(aes(y = sma_10), color = "blue", size = 0.8, na.rm = TRUE)
      }
      if (input$show_sma20) {
        p <- p + geom_line(aes(y = sma_20), color = "orange", size = 0.8, na.rm = TRUE)
      }
      
      p <- p +
        labs(title = paste(symbol, "- Price Chart"),
             y = "Price ($)", x = NULL) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 12),
          panel.grid.minor = element_blank()
        )
      
      print(p)
    })
    
    # Volume Chart
    output$volume_chart <- renderPlot({
      # Calculate volume color based on price change
      df_calc <- df_calc %>%
        mutate(volume_color = ifelse(close >= open, "up", "down"))
      
      ggplot(df_calc, aes(x = date, y = volume)) +
        geom_bar(aes(fill = volume_color), stat = "identity") +
        scale_fill_manual(values = c("up" = "#00CC66", "down" = "#FF3333"), guide = "none") +
        geom_line(aes(y = avg_volume_10), color = "blue", size = 0.8, na.rm = TRUE) +
        labs(title = "Volume",
             y = "Volume", x = NULL) +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11),
          panel.grid.minor = element_blank()
        )
    })
    
    # Conditions Score Chart
    output$conditions_chart <- renderPlot({
      ggplot(df_calc, aes(x = date, y = conditions_met)) +
        geom_bar(aes(fill = condition_strength), stat = "identity") +
        scale_fill_manual(name = "Strength",
                         values = c("Strong" = "#00CC66", "Moderate" = "#FFA500", "Weak" = "#FF3333")) +
        geom_hline(yintercept = 5, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
        geom_hline(yintercept = 3, linetype = "dashed", color = "orange", alpha = 0.7) +
        labs(title = "SOS Conditions Met (out of 6)",
             y = "# Conditions", x = "Date") +
        ylim(0, 6) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 10),
          axis.title = element_text(size = 11),
          legend.position = "top",
          panel.grid.minor = element_blank()
        )
    })
    
    # Analysis text
    output$stock_analysis <- renderPrint({
      latest <- tail(df_calc, 1)
      prev <- tail(df_calc, 2)[1, ]
      
      cat("=== Technical Analysis for", symbol, "===\n\n")
      cat("Date:", format(latest$date, "%Y-%m-%d"), "\n\n")
      
      cat("--- Price Information ---\n")
      cat("Close:", sprintf("$%.2f", latest$close), "\n")
      cat("Open:", sprintf("$%.2f", latest$open), "\n")
      cat("High:", sprintf("$%.2f", latest$high), "\n")
      cat("Low:", sprintf("$%.2f", latest$low), "\n")
      cat("Change:", sprintf("%.2f%%", latest$pct_change), "\n\n")
      
      cat("--- Moving Averages ---\n")
      cat("10-day SMA:", sprintf("$%.2f", latest$sma_10), "\n")
      cat("20-day SMA:", sprintf("$%.2f", latest$sma_20), "\n")
      cat("Price vs 10-day SMA:", sprintf("%.2f%%", (latest$close - latest$sma_10) / latest$sma_10 * 100), "\n\n")
      
      cat("--- Momentum Indicators ---\n")
      cat("Range Expansion:", sprintf("%.2fx", latest$range_expansion), 
          ifelse(latest$cond_range_expansion == 1, "✓", "✗"), "\n")
      cat("Volume Surge:", sprintf("%.2fx", latest$volume_surge),
          ifelse(latest$cond_volume_surge == 1, "✓", "✗"), "\n")
      cat("Consecutive Up Days:", latest$consecutive_up,
          ifelse(latest$cond_not_extended == 1, "✓", "✗"), "\n")
      cat("Gain from 20d Low:", sprintf("%.2f%%", latest$gain_from_20d_low), "\n\n")
      
      cat("--- SOS Conditions Check ---\n")
      cat("1. Range Expansion (≥1.5x):", ifelse(latest$cond_range_expansion == 1, "✓ MET", "✗ NOT MET"), "\n")
      cat("2. Volume Surge (≥1.2x):", ifelse(latest$cond_volume_surge == 1, "✓ MET", "✗ NOT MET"), "\n")
      cat("3. Price Up (>2%):", ifelse(latest$cond_price_increase == 1, "✓ MET", "✗ NOT MET"), "\n")
      cat("4. Above 10-day SMA:", ifelse(latest$cond_above_sma10 == 1, "✓ MET", "✗ NOT MET"), "\n")
      cat("5. Above 20-day SMA:", ifelse(latest$cond_above_sma20 == 1, "✓ MET", "✗ NOT MET"), "\n")
      cat("6. Not Extended (≤2 days):", ifelse(latest$cond_not_extended == 1, "✓ MET", "✗ NOT MET"), "\n")
      cat("\nTotal Conditions Met:", latest$conditions_met, "/ 6\n")
      cat("Strength:", latest$condition_strength, "\n\n")
      
      cat("--- Stochastic (8,3,3) ---\n")
      cat("Stochastic K:", sprintf("%.2f", latest$stoch_k), "\n")
      cat("Stochastic D:", sprintf("%.2f", latest$stoch_d), "\n")
      if (!is.na(latest$stoch_k)) {
        if (latest$stoch_k > 80) {
          cat("Status: OVERBOUGHT (>80)\n")
        } else if (latest$stoch_k < 20) {
          cat("Status: OVERSOLD (<20)\n")
        } else {
          cat("Status: NEUTRAL (20-80)\n")
        }
      }
      cat("\n")
      
      cat("--- Stop Loss Calculation ---\n")
      cat("Today's Range:", sprintf("$%.2f", latest$range), "\n")
      cat("Suggested Stop (Half-Range):", sprintf("$%.2f", latest$close - latest$range/2), "\n")
      cat("Risk per Share:", sprintf("$%.2f", latest$range/2), "\n")
    })
  })
}

# Run app
shinyApp(ui = ui, server = server)