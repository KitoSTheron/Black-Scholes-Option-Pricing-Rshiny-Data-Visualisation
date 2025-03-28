library(shiny)
library(ggplot2)
library(reshape2)
library(plotly)


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
  output$interactive_plot_call <- renderPlotly({
    n <- 15  # Reduce the number of points to avoid memory issues
    
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
    
    z_var <- expand.grid(x_var = x_var, y_var = y_var)
    z_var$call_price <- mapply(function(x, y) {
      args <- list(stock_price = input$stockPrice,
                   strike_price = input$strikePrice,
                   time_to_expiration = input$timeToExpiration,
                   risk_free_rate = input$riskFreeRateSlider,
                   volatility = input$volatilitySlider)
      args[[input$xAxis]] <- x
      args[[input$yAxis]] <- y
      do.call(black_scholes, args)$call
    }, z_var$x_var, z_var$y_var)
    
    plot_ly(z_var, x = ~x_var, y = ~y_var, z = ~call_price, type = 'scatter3d', mode = 'markers', marker = list(size = 3, color = ~call_price, colorscale = list(c(0, 'darkred'), c(0.5, 'darkorange'), c(1, 'darkgreen')))) %>%
      layout(title = "Call Option 3D Scatter Plot",
        scene = list(xaxis = list(title = input$xAxis, backgroundcolor = "#343a40", color = "#f8f9fa", gridcolor = "#4e5d6c"),
                yaxis = list(title = input$yAxis, backgroundcolor = "#343a40", color = "#f8f9fa", gridcolor = "#4e5d6c"),
                zaxis = list(title = 'Call Price', backgroundcolor = "#343a40", color = "#f8f9fa", gridcolor = "#4e5d6c")),
         paper_bgcolor = "#343a40",
         plot_bgcolor = "#343a40",
         font = list(color = "#f8f9fa")
         )
  })

  output$interactive_plot_put <- renderPlotly({
    n <- 15  # Reduce the number of points to avoid memory issues
    
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
    
    z_var <- expand.grid(x_var = x_var, y_var = y_var)
    z_var$put_price <- mapply(function(x, y) {
      args <- list(stock_price = input$stockPrice,
                   strike_price = input$strikePrice,
                   time_to_expiration = input$timeToExpiration,
                   risk_free_rate = input$riskFreeRateSlider,
                   volatility = input$volatilitySlider)
      args[[input$xAxis]] <- x
      args[[input$yAxis]] <- y
      do.call(black_scholes, args)$put
    }, z_var$x_var, z_var$y_var)
    
    plot_ly(z_var, x = ~x_var, y = ~y_var, z = ~put_price, type = 'scatter3d', mode = 'markers', marker = list(size = 3, color = ~put_price, colorscale = list(c(0, 'darkred'), c(0.5, 'darkorange'), c(1, 'darkgreen')))) %>%
      layout(title = "Put Option 3D Scatter Plot",
         scene = list(xaxis = list(title = input$xAxis, backgroundcolor = "#343a40", color = "#f8f9fa", gridcolor = "#4e5d6c"),
                          yaxis = list(title = input$yAxis, backgroundcolor = "#343a40", color = "#f8f9fa", gridcolor = "#4e5d6c"),
                          zaxis = list(title = 'Put Price', backgroundcolor = "#343a40", color = "#f8f9fa", gridcolor = "#4e5d6c")),
             paper_bgcolor = "#343a40",
             plot_bgcolor = "#343a40",
             font = list(color = "#f8f9fa"))
  })

  output$parallel_plot <- renderPlotly({
    n <- 20  # Number of data points
    
    # Create base data frame with all variables
    data <- data.frame(
      x_axis = seq(0, 2 * input$strikePrice, length.out = n),  # Use x-axis value as the first column
      stock_price = rep(input$stockPrice, n),
      strike_price = rep(input$strikePrice, n),
      time_to_expiration = rep(input$timeToExpiration, n),
      risk_free_rate = rep(input$riskFreeRateSlider, n),
      volatility = rep(input$volatilitySlider, n)
    )
    
    # Calculate option prices for each point
    prices <- mapply(function(x) {
      result <- black_scholes(
        stock_price = x,
        strike_price = input$strikePrice,
        time_to_expiration = input$timeToExpiration,
        risk_free_rate = input$riskFreeRateSlider,
        volatility = input$volatilitySlider
      )
      c(result$call, result$put)
    }, data$x_axis)
    
    data$call_price <- prices[1,]
    data$put_price <- prices[2,]
    
    plot_ly() %>%
      add_trace(
        type = 'parcoords',
        line = list(color = data$x_axis,
                   colorscale = list(c(0,'darkred'), c(0.5,'darkorange'), c(1,'darkgreen'))),
        dimensions = list(
          list(range = range(data$x_axis), label = input$xAxis, values = data$x_axis),
          list(range = range(data$stock_price), label = 'Stock Price', values = data$stock_price),
          list(range = range(data$strike_price), label = 'Strike Price', values = data$strike_price),
          list(range = range(data$time_to_expiration), label = 'Time to Expiration', values = data$time_to_expiration),
          list(range = range(data$risk_free_rate), label = 'Risk-Free Rate', values = data$risk_free_rate),
          list(range = range(data$volatility), label = 'Volatility', values = data$volatility),
          list(range = range(data$call_price), label = 'Call Price', values = data$call_price),
          list(range = range(data$put_price), label = 'Put Price', values = data$put_price)
        )
      ) %>%
      layout(
        title = "Parallel Coordinates Plot",
        paper_bgcolor = "#343a40",
        plot_bgcolor = "#343a40",
        font = list(color = "#f8f9fa")
      )
  })

}
