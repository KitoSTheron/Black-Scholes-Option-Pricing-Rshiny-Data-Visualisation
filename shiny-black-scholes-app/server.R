library(shiny)

# Black-Scholes function
black_scholes <- function(stock_price, strike_price, time_to_expiration, risk_free_rate, volatility) {
  d1 <- (log(stock_price / strike_price) + (risk_free_rate + volatility^2 / 2) * time_to_expiration) / (volatility * sqrt(time_to_expiration))
  d2 <- d1 - volatility * sqrt(time_to_expiration)
  
  call_price <- stock_price * pnorm(d1) - strike_price * exp(-risk_free_rate * time_to_expiration) * pnorm(d2)
  put_price <- strike_price * exp(-risk_free_rate * time_to_expiration) * pnorm(-d2) - stock_price * pnorm(-d1)
  
  return(list(call = call_price, put = put_price))
}

server <- function(input, output) {
  
  option_price <- reactive({
    stock_price <- input$stockPrice
    strike_price <- input$strikePrice
    time_to_expiration <- input$timeToExpiration
    risk_free_rate <- input$riskFreeRateSlider
    volatility <- input$volatilitySlider
    
    black_scholes(stock_price, strike_price, time_to_expiration, risk_free_rate, volatility)
  })
  
  output$call_price <- renderText({
    paste("Call Option Price: £", round(option_price()$call, 2))
  })
  
  output$put_price <- renderText({
    paste("Put Option Price: £", round(option_price()$put, 2))
  })
  
  output$heatmapPlotCall <- renderPlot({
    stock_prices <- seq(0, 2 * input$strikePrice, length.out = 100)
    volatilities <- seq(0.01, 1, length.out = 100)
    
    call_prices <- outer(stock_prices, volatilities, Vectorize(function(S, V) {
    black_scholes(S, input$strikePrice, input$timeToExpiration, input$riskFreeRateSlider, V)$call
      }))
    
    filled.contour(stock_prices, volatilities, call_prices, color.palette = colorRampPalette(c("red", "yellow", "green")),
                    xlab = "Stock Price", ylab = "Volatility", main = "Call Option Prices Heatmap")
    })

  
  output$heatmapPlotPut <- renderPlot({
    stock_prices <- seq(0, 2 * input$strikePrice, length.out = 100)
    volatilities <- seq(0.01, 1, length.out = 100)
    
    put_prices <- outer(stock_prices, volatilities, Vectorize(function(S, V) {
      black_scholes(S, input$strikePrice, input$timeToExpiration, input$riskFreeRateSlider, V)$put
    }))
    
    filled.contour(stock_prices, volatilities, put_prices, color.palette = colorRampPalette(c("red", "yellow", "green")),
                   xlab = "Stock Price", ylab = "Volatility", main = "Put Option Prices Heatmap")
  })
}
