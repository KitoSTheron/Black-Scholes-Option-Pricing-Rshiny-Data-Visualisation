# Customisable Option Pricing Visualisation using the Black-Scholes Model

## Overview
This Shiny application provides an interactive and visual way to explore the pricing of European-style call and put options using the Black-Scholes model. The app includes a variety of customisable graphs that allow users to analyse how different parameters affect option prices. 

### Features
- **3D Scatter Plots**: Visualise the relationship between two parameters (e.g., stock price and volatility) and the resulting call or put option prices.
- **Heatmaps**: Explore the option price distribution across two parameters, with colour gradients representing price levels.
- **Parallel Coordinates Plot**: Analyse the combined impact of multiple parameters (e.g., stock price, volatility, time to expiration) on call and put prices.

### Customisation
The graphs are fully customisable:
- Select the **Value 1** and **Value 2** parameters (e.g., stock price, volatility, risk-free rate, time to expiration, or strike price).
- Adjust input parameters such as:
  - **Stock Price (S)**: The current price of the underlying stock.
  - **Strike Price (K)**: The price at which the option can be exercised.
  - **Time to Expiration (T)**: The time remaining until the option expires.
  - **Risk-Free Rate (r)**: The rate of return of a risk-free investment.
  - **Volatility (σ)**: The degree of variation in the stock's price over time.

These customisations allow users to tailor the visualisations to their specific needs and gain deeper insights into option pricing dynamics.

---

## Graphs Included
1. **3D Scatter Plots**:
   - One for call option prices.
   - One for put option prices.
   - These plots show how option prices change with two selected parameters.

2. **Heatmaps**:
   - One for call option prices.
   - One for put option prices.
   - Heatmaps provide a clear visual representation of price levels across two parameters.

3. **Parallel Coordinates Plot**:
   - This plot allows users to analyse the combined effect of multiple parameters on option prices, providing a holistic view of the pricing model.

---

## How to Run the Programme
### Option 1: Run Online
You can access the application directly via the following URL:
[https://kitotheron.shinyapps.io/Customisable_Option_Pricing_Visualisation/](https://kitotheron.shinyapps.io/Customisable_Option_Pricing_Visualisation/)

### Option 2: Run Locally
To run the application locally, follow these steps:
1. Clone or download the repository to your local machine.
2. Ensure you have R and the required packages installed. The key packages include:
   - `shiny`
   - `plotly`
   - `ggplot2`
   - `bslib`
   - `gridlayout` (install from GitHub if necessary using `remotes::install_github("rstudio/gridlayout")`)
3. Open the R console or RStudio and set the working directory to the folder containing the app.
4. Run the following command:
   ```r
   shiny::runApp()