library(shiny)
library(ggplot2)
library(reshape2)

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
  
  output$heatmap_plot_call <- renderPlot({
    n <- 10  # Adjust the number of points based on window size
    
    x_var <- switch(input$xAxis,
                    "stock_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    "volatility" = seq(0.01, 1, length.out = n),
                    "risk_free_rate" = seq(0, 1, length.out = n),
                    "time_to_expiration" = seq(0.01, 2, length.out = n),
                    "strike_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    stop("Invalid xAxis value"))
    
    y_var <- switch(input$yAxis,
                    "stock_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    "volatility" = seq(0.01, 1, length.out = n),
                    "risk_free_rate" = seq(0, 1, length.out = n),
                    "time_to_expiration" = seq(0.01, 2, length.out = n),
                    "strike_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    stop("Invalid yAxis value"))
    
    call_prices <- expand.grid(x_var = x_var, y_var = y_var)
    call_prices$call_price <- mapply(function(x, y) {
      args <- list(stock_price = input$stockPrice,
                   strike_price = input$strikePrice,
                   time_to_expiration = input$timeToExpiration,
                   risk_free_rate = input$riskFreeRateSlider,
                   volatility = input$volatilitySlider)
      args[[input$xAxis]] <- x
      args[[input$yAxis]] <- y
      do.call(black_scholes, args)$call
    }, call_prices$x_var, call_prices$y_var)
    
    ggplot(call_prices, aes(x = x_var, y = y_var, fill = call_price)) +
      geom_tile() +
      geom_text(aes(label = round(call_price, 2)), color = "white", size = 3) +
      scale_fill_gradientn(colors = c("darkred", "darkorange", "darkgreen")) +
      guides(fill = guide_colorbar(barheight = unit(0.75, "npc"))) +
      labs(title = "Call Option Prices Heatmap", x = input$xAxis, y = input$yAxis) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#343a40", color = NA),
        panel.background = element_rect(fill = "#343a40", color = NA),
        panel.grid.major = element_line(color = "#4e5d6c"),
        panel.grid.minor = element_line(color = "#4e5d6c"),
        axis.text = element_text(color = "#f8f9fa"),
        axis.title = element_text(color = "#f8f9fa"),
        plot.title = element_text(color = "#f8f9fa"),
        legend.background = element_rect(fill = "#343a40", color = NA),
        legend.text = element_text(color = "#f8f9fa"),
        legend.title = element_text(color = "#f8f9fa")
      )
  })
  
  output$heatmap_plot_put <- renderPlot({
    n <- 10  # Reduce the number of points to avoid memory issues
    
    x_var <- switch(input$xAxis,
                    "stock_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    "volatility" = seq(0.01, 1, length.out = n),
                    "risk_free_rate" = seq(0, 1, length.out = n),
                    "time_to_expiration" = seq(0.01, 2, length.out = n),
                    "strike_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    stop("Invalid xAxis value"))
    
    y_var <- switch(input$yAxis,
                    "stock_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    "volatility" = seq(0.01, 1, length.out = n),
                    "risk_free_rate" = seq(0, 1, length.out = n),
                    "time_to_expiration" = seq(0.01, 2, length.out = n),
                    "strike_price" = seq(0, 2 * input$strikePrice, length.out = n),
                    stop("Invalid yAxis value"))
    
    put_prices <- expand.grid(x_var = x_var, y_var = y_var)
    put_prices$put_price <- mapply(function(x, y) {
      args <- list(stock_price = input$stockPrice,
                   strike_price = input$strikePrice,
                   time_to_expiration = input$timeToExpiration,
                   risk_free_rate = input$riskFreeRateSlider,
                   volatility = input$volatilitySlider)
      args[[input$xAxis]] <- x
      args[[input$yAxis]] <- y
      do.call(black_scholes, args)$put
    }, put_prices$x_var, put_prices$y_var)
    
    ggplot(put_prices, aes(x = x_var, y = y_var, fill = put_price)) +
      geom_tile() +
      geom_text(aes(label = round(put_price, 2)), color = "white", size = 3) +
      scale_fill_gradientn(colors = c("darkred", "darkorange", "darkgreen")) +
      guides(fill = guide_colorbar(barheight = unit(0.75, "npc"))) +
      labs(title = "Put Option Prices Heatmap", x = input$xAxis, y = input$yAxis) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#343a40", color = NA),
        panel.background = element_rect(fill = "#343a40", color = NA),
        panel.grid.major = element_line(color = "#4e5d6c"),
        panel.grid.minor = element_line(color = "#4e5d6c"),
        axis.text = element_text(color = "#f8f9fa"),
        axis.title = element_text(color = "#f8f9fa"),
        plot.title = element_text(color = "#f8f9fa"),
        legend.background = element_rect(fill = "#343a40", color = NA),
        legend.text = element_text(color = "#f8f9fa"),
        legend.title = element_text(color = "#f8f9fa")
      )
  })
}
