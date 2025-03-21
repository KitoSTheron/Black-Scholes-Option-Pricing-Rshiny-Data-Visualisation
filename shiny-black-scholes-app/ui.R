library(shiny)
library(plotly)
library(gridlayout)
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
ui <- page_navbar(
  title = "European Option Pricing",
  selected = "Option Pricing",
  collapsible = TRUE,
  theme = dark_theme,
  
  # Main App Panel
  nav_panel(
    title = "Option Pricing",
    grid_page(
      theme = dark_theme,
      
      # Define the grid layout as lines describing each row
      layout = c(
        "header   header",
        "sidebar  main"
      ),
      # Row and Column sizes
      row_sizes = c("60px", "1fr"),
      col_sizes = c("300px", "1fr"),
      gap_size = "1rem",
      
      # Header
      grid_card_text(
        area = "header",
        content = "European Option Pricing using the Black-Scholes Model",
        alignment = "start",
        is_title = TRUE
      ),
      
      # Sidebar
      grid_card(
        area = "sidebar",
        card_header("Input Parameters"),
        card_body(
          numericInput("stockPrice", "Stock Price (S):", value = 100),
          p("The current price of the underlying stock."),
          
          numericInput("strikePrice", "Strike Price (K):", value = 100),
          p("The price at which the option can be exercised."),
          
          numericInput("timeToExpiration", "Time to Expiration (T in years):", value = 1),
          p("The time remaining until the option's expiration, in years."),
          
          sliderInput("riskFreeRateSlider", "Risk-Free Rate (r):", 
                      min = 0, max = 1, value = 0.05, step = 0.01),
          p("The theoretical rate of return of an investment with zero risk."),
          
          sliderInput("volatilitySlider", "Volatility (σ):", 
                      min = 0, max = 1, value = 0.2, step = 0.01),
          p("The volatility of the stock's returns, as a decimal."),
          
          selectInput("xAxis", "Axis Value 1:", 
            choices = c("Stock Price" = "stock_price", 
                        "Volatility" = "volatility", 
                        "Risk-Free Rate" = "risk_free_rate", 
                        "Time to Expiration" = "time_to_expiration", 
                        "Strike Price" = "strike_price"), 
            selected = "stock_price"),
          selectInput("yAxis", "Axis Value 2:", 
            choices = c("Stock Price" = "stock_price", 
                        "Volatility" = "volatility", 
                        "Risk-Free Rate" = "risk_free_rate", 
                        "Time to Expiration" = "time_to_expiration", 
                        "Strike Price" = "strike_price"), 
            selected = "volatility")
        )
      ),
      
      # Main content
      grid_card(
        area = "main",
        card_header("Option Price Results"),
        card_body(
          fluidRow(
            column(6, verbatimTextOutput("call_price", placeholder = FALSE)),
            column(6, verbatimTextOutput("put_price", placeholder = FALSE))
          ),
          grid_container(
            layout = c(
              "plot1 plot2",
              "plot3 plot4"
            ),
            row_sizes = c("1fr", "1fr"),
            col_sizes = c("1fr", "1fr"),
            gap_size = "10px",
            grid_card("plot1", plotlyOutput("interactive_plot_call"), aspect_ratio = 0.9),
            grid_card("plot2", plotlyOutput("interactive_plot_put"), aspect_ratio = 0.9),
            grid_card("plot3", plotOutput("heatmap_plot_call"), aspect_ratio = 0.9),
            grid_card("plot4", plotOutput("heatmap_plot_put"), aspect_ratio = 0.9)
          )
        )
      )
    )
  ),
  
  # Information Tab
  nav_panel(
    title = "About Options",
    fluidPage(
      h2("Introduction to Options & the Black-Scholes Model"),
      p("Options are financial derivatives that give the holder the right, but not the obligation, to buy or sell an underlying asset at a specified strike price before or on a particular date."),
      h3("Call and Put Options"),
      p("A call option gives the holder the right to buy the underlying asset, whereas a put option gives the holder the right to sell it."),
      
      h3("The Black-Scholes Model"),
      p("The Black-Scholes model is a mathematical model for pricing European-style options. It provides a theoretical estimate of the price of options and is widely used in financial markets."),
      
      p("Key assumptions include a lognormal distribution of stock prices, constant volatility, and no dividends during the option's life."),
      
      h4("The Black-Scholes Formula"),
      p("Call Price: C = S⋅N(d₁) - K⋅e^(-rT)⋅N(d₂)"),
      p("Put Price: P = K⋅e^(-rT)⋅N(-d₂) - S⋅N(-d₁)"),
      p("where:"),
      p("d₁ = [ln(S/K) + (r + σ^2/2)T] / (σ√T)"),
      p("d₂ = d₁ - σ√T"),
      p("S = Stock Price, K = Strike Price, T = Time to Expiration, r = Risk-Free Rate, σ = Volatility, N(x) = CDF of standard normal distribution.")
    )
  )
)

# Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1000))