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
    risk_free_rate <- input$riskFreeRate
    volatility <- input$volatility
    
    black_scholes(stock_price, strike_price, time_to_expiration, risk_free_rate, volatility)
  })
  
  output$call_price <- renderText({
    paste("Call Option Price: $", round(option_price()$call, 2))
  })
  
  output$put_price <- renderText({
    paste("Put Option Price: $", round(option_price()$put, 2))
  })
  
  output$pricePlot <- renderPlot({
    stock_prices <- seq(0, 2 * input$strikePrice, length.out = 100)
    prices <- sapply(stock_prices, function(stock_price) {
      black_scholes(stock_price, input$strikePrice, input$timeToExpiration, input$riskFreeRate, input$volatility)
    })
    
    plot(stock_prices, prices[1, ], type = "l", col = "blue", ylim = c(0, max(prices)), 
         xlab = "Stock Price", ylab = "Option Price", main = "European Option Prices")
    lines(stock_prices, prices[2, ], col = "red")
    legend("topright", legend = c("Call Option", "Put Option"), col = c("blue", "red"), lty = 1)
  })
  
  output$heatmapPlot <- renderPlot({
    stock_prices <- seq(0, 2 * input$strikePrice, length.out = 100)
    times_to_expiration <- seq(0.01, 2, length.out = 100)
    
    call_prices <- outer(stock_prices, times_to_expiration, Vectorize(function(S, T) {
      black_scholes(S, input$strikePrice, T, input$riskFreeRate, input$volatility)$call
    }))
    
    filled.contour(stock_prices, times_to_expiration, call_prices, color.palette = terrain.colors,
                   xlab = "Stock Price", ylab = "Time to Expiration", main = "Call Option Prices Heatmap")
  })
}