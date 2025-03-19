# Shiny Black-Scholes Option Pricing App

This project is an R Shiny application that visualizes the pricing of European options using the Black-Scholes model. Users can customize various input parameters through a sidebar, and the visualizations update in real time based on these inputs.

## Files Overview

- **server.R**: Contains the server logic for the Shiny application. It processes user inputs from the UI, calculates the option prices using the Black-Scholes model, and updates the visualizations in real time.

- **ui.R**: Defines the user interface of the Shiny application. It includes a sidebar for customizable inputs such as stock price, strike price, time to expiration, risk-free rate, and volatility. It also contains output elements for displaying the calculated option prices and visualizations.

## How to Run the App

1. Ensure you have R and RStudio installed on your machine.
2. Install the required packages if you haven't already:

   ```R
   install.packages(c("shiny", "ggplot2"))
   ```

3. Open the `ui.R` file in RStudio.
4. Click on the "Run App" button in RStudio, or run the following command in the R console:

   ```R
   shiny::runApp()
   ```

5. The application will launch in your default web browser.

## Black-Scholes Model

The Black-Scholes model is a mathematical model used for pricing European-style options. It calculates the theoretical price of options based on several factors, including:

- **Stock Price (S)**: The current price of the underlying asset.
- **Strike Price (K)**: The price at which the option can be exercised.
- **Time to Expiration (T)**: The time remaining until the option expires, expressed in years.
- **Risk-Free Rate (r)**: The theoretical return on an investment with zero risk, typically represented by government bonds.
- **Volatility (σ)**: A measure of the price fluctuations of the underlying asset.

## Input Parameters

- **Stock Price**: Enter the current price of the underlying asset.
- **Strike Price**: Enter the strike price of the option.
- **Time to Expiration**: Enter the time remaining until expiration (in years).
- **Risk-Free Rate**: Enter the risk-free interest rate (as a decimal).
- **Volatility**: Enter the volatility of the underlying asset (as a decimal).
