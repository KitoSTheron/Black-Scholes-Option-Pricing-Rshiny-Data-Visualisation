library(shiny)
library(bslib)

# Source the server file
source("server.R")

# Define a dark theme using bslib
dark_theme <- bs_theme(
  bg = "#343a40", 
  fg = "#f8f9fa", 
  primary = "#007bff", 
  secondary = "#6c757d", 
  success = "#28a745", 
  info = "#17a2b8", 
  warning = "#ffc107", 
  danger = "#dc3545", 
  base_font = font_google("Roboto")
)

# Define UI for the Black-Scholes option pricing app
ui <- fluidPage(
  theme = dark_theme,
  
  # Application title
  titlePanel(h1("European Option Pricing using the Black-Scholes Model", align = "center")),
  
  # Sidebar layout with input and output definitions
  fluidRow(
    
    # Sidebar panel for inputs
    column(4,
      wellPanel(
        numericInput("stockPrice", "Stock Price (S):", value = 100),
        p("The current price of the underlying stock."),
        
        numericInput("strikePrice", "Strike Price (K):", value = 100),
        p("The price at which the option can be exercised."),
        
        numericInput("timeToExpiration", "Time to Expiration (T in years):", value = 1),
        p("The time remaining until the option's expiration, in years."),
        
        sliderInput("riskFreeRateSlider", "Risk-Free Rate (r):", min = 0, max = 1, value = 0.05, step = 0.01),
        p("The risk-free interest rate, as a decimal."),
        
        sliderInput("volatilitySlider", "Volatility (σ):", min = 0, max = 1, value = 0.2, step = 0.01),
        p("The volatility of the stock's returns, as a decimal."),
        
        
      )
    ),
    
    # Main panel for displaying outputs
    column(8,
      h3("Option Price Results"),
      verbatimTextOutput("call_price"),
      verbatimTextOutput("put_price"),
      fluidRow(
        selectInput("xAxis", "X-Axis:", choices = c("Stock Price" = "stock_price", "Volatility" = "volatility", "Risk-Free Rate" = "risk_free_rate", "Time to Expiration" = "time_to_expiration", "Strike Price" = "strike_price"), selected = "stock_price"),
        selectInput("yAxis", "Y-Axis:", choices = c("Stock Price" = "stock_price", "Volatility" = "volatility", "Risk-Free Rate" = "risk_free_rate", "Time to Expiration" = "time_to_expiration", "Strike Price" = "strike_price"), selected = "volatility")
      ),
      fluidRow(
        column(6, plotOutput("heatmap_plot_call", height = "400px")),
        column(6, plotOutput("heatmap_plot_put", height = "400px"))
      )
    )
  ),
  
  # Display the Black-Scholes equation
  fluidRow(
    column(12,
      h4("Black-Scholes Equation:"),
      p("Call Option (right to buy) Price: C = Current Stock Price * Cumulative Distribution Function of d1 - Strike Price * Exponential Function of (-Risk-Free Rate * Time to Expiration) * Cumulative Distribution Function of d2"),
      p("Put Option (right to sell) Price: P = Strike Price * Exponential Function of (-Risk-Free Rate * Time to Expiration) * Cumulative Distribution Function of -d2 - Current Stock Price * Cumulative Distribution Function of -d1"),
      p("where:"),
      p("d1 = (Natural Logarithm of (Current Stock Price / Strike Price) + (Risk-Free Rate + (Volatility Squared / 2)) * Time to Expiration) / (Volatility * Square Root of Time to Expiration)"),
      p("d2 = d1 - Volatility * Square Root of Time to Expiration")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 2000))