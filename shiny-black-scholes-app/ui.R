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
  titlePanel("European Option Pricing using the Black-Scholes Model"),
  
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
      plotOutput("heatmapPlotCall", height = "400px"),
      plotOutput("heatmapPlotPut", height = "400px")
    ),
    
    # Display the Black-Scholes equation
    fluidRow(
      column(12,
             h4("Black-Scholes Equation:"),
             p("Call Option Price: C = S * N(d1) - K * exp(-r * T) * N(d2)"),
             p("Put Option Price: P = K * exp(-r * T) * N(-d2) - S * N(-d1)"),
             p("where:"),
             p("d1 = (log(S / K) + (r + σ^2 / 2) * T) / (σ * sqrt(T))"),
             p("d2 = d1 - σ * sqrt(T)")
      )
    ),
  )
)

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1080))