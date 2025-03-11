library(shiny)
library(bslib)
library(plotly)

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
ui <- navbarPage(
  title = div(h1("European Option Pricing using the Black-Scholes Model")),
  theme = dark_theme,
  
  tabPanel("Inputs",
    fluidRow(
      column(3,
        wellPanel(
          h3("Input Parameters"),
          numericInput("stockPrice", "Stock Price (S):", value = 100),
          p("Current price of the underlying stock."),
          
          numericInput("strikePrice", "Strike Price (K):", value = 100),
          p("Price at which the option can be exercised."),
          
          numericInput("timeToExpiration", "Time to Expiration (T in years):", value = 1),
          p("Time remaining until the option's expiration, in years."),
          
          sliderInput("riskFreeRateSlider", "Risk-Free Rate (r):", min = 0, max = 1, value = 0.05, step = 0.01),
              p("The theoretical rate of return of an investment with zero risk."),
          
          sliderInput("volatilitySlider", "Volatility (σ):", min = 0, max = 1, value = 0.2, step = 0.01),
          p("Volatility of the stock's returns, as a decimal."),
          
          selectInput("xAxis", "Axis Value 1:", choices = c("Stock Price" = "stock_price", "Volatility" = "volatility", "Risk-Free Rate" = "risk_free_rate", "Time to Expiration" = "time_to_expiration", "Strike Price" = "strike_price"), selected = "stock_price"),
          selectInput("yAxis", "Axis Value 2:", choices = c("Stock Price" = "stock_price", "Volatility" = "volatility", "Risk-Free Rate" = "risk_free_rate", "Time to Expiration" = "time_to_expiration", "Strike Price" = "strike_price"), selected = "volatility")
        )
      ),
      
      column(9,
        h3("Option Price Results"),
        fluidRow(
          column(6, verbatimTextOutput("call_price")),
          column(6, verbatimTextOutput("put_price"))
        ),
        fluidRow(
          column(6, plotlyOutput("interactive_plot_call", height = "700px",width = "700px")),
          column(6, plotlyOutput("interactive_plot_put", height = "700px",width = "700px"))
        ),
        fluidRow(
          column(6, plotOutput("heatmap_plot_call", height = "400px")),
          column(6, plotOutput("heatmap_plot_put", height = "400px"))
        )
      )
    )
  ),
    
  tabPanel("Black-Scholes Equation",
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
)

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 2000))