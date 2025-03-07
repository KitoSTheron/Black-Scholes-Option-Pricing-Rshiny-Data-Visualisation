library(shiny)

# Define UI for the Black-Scholes option pricing app
ui <- fluidPage(
  
  # Application title
  titlePanel("European Option Pricing using the Black-Scholes Model"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      numericInput("stockPrice", "Stock Price (S):", value = 100, min = 0),
      numericInput("strikePrice", "Strike Price (K):", value = 100, min = 0),
      numericInput("timeToExpiration", "Time to Expiration (T in years):", value = 1, min = 0),
      numericInput("riskFreeRate", "Risk-Free Rate (r):", value = 0.05, min = 0, max = 1),
      numericInput("volatility", "Volatility (σ):", value = 0.2, min = 0, max = 1),
      actionButton("calculate", "Calculate Option Price")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      h3("Option Price Results"),
      verbatimTextOutput("call_price"),
      verbatimTextOutput("put_price"),
      plotOutput("pricePlot"),
      plotOutput("heatmapPlot")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = function(input, output) {})