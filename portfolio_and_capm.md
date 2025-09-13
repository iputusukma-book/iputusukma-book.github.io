---
layout: default
---

## Modern Portfolio Theory and Capital Asset Pricing Model
------

* The presentation and lecture notes for modern portfolio theory and introduction to Capital Asset Pricing Model could be acessed through this [link](https://iputusukma-book.github.io/fintheory_markowitz_and_sharpeCAPM.pdf).
* The Harry Markowitz (1952) and Sharpe (1964) are available in this [repository](https://s.id/ipsh_fintheory_odd25). You also could access additional material regarding CAPM and single-index model from [Prof. Alex Shapiro](https://pages.stern.nyu.edu/~ashapiro/) in the same repository.
* In order to give you a hands-on material to understand the portfolio construction using 2-assets and 3-assets as well as employing both randomize approach and linear approach, I create an excel spreadsheet containing those portfolio simulation and visual depiction of Capital Market Line (CML) and Security Market Line (SML) that could be accessed through this [link](https://github.com/iputusukma-book/iputusukma-book.github.io/blob/31b6c003c59c395cd7de0ff231147fb7af2ecabe/ipsh_fintheory_asset%20pricing%20in%20excel.xlsx).

```R
# --- 1. Install and load necessary packages ---
# If you don't have these packages installed, uncomment and run the lines below.
# install.packages("quantmod")
# install.packages("PerformanceAnalytics")
# install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("magrittr")

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)

# --- 2. Define the assets and risk-free rate ---
# These are the stock tickers and the risk-free rate (Indonesia 10-year T-bond rate).
# Note: The risk-free rate is a hypothetical value for demonstration.
tickers <- c("BBCA.JK", "TLKM.JK", "UNVR.JK", "ASII.JK", "ICBP.JK", "BMRI.JK", "BRPT.JK", "PGAS.JK", "INDF.JK", "ADRO.JK")
risk_free_rate <- 0.065 # Assumption: 6.5% annual rate, as of a recent market observation.

# --- 3. Fetch data and calculate returns ---
# We'll download the adjusted close prices for the last 5 years.
start_date <- as.Date("2019-01-01")
end_date <- Sys.Date()

# Create a list to store the price data for each ticker.
prices <- new.env()
getSymbols(tickers, env = prices, from = start_date, to = end_date)

# Calculate monthly returns.
returns <- do.call(merge, lapply(tickers, function(ticker) {
  monthlyReturn(Ad(prices[[ticker]]))
}))
colnames(returns) <- tickers

# Remove any rows with missing data.
returns <- na.omit(returns)

# --- 4. Prepare for Efficient Frontier calculation ---
# Calculate the mean returns and covariance matrix.
mean_returns <- colMeans(returns)
cov_matrix <- cov(returns)

# Number of assets in the portfolio.
num_assets <- length(tickers)

# Number of random portfolios to simulate. A larger number gives a smoother curve.
num_portfolios <- 10000

# Create matrices to store the results.
portfolio_returns <- vector('numeric', num_portfolios)
portfolio_sd <- vector('numeric', num_portfolios)
portfolio_sharpe <- vector('numeric', num_portfolios)
portfolio_weights <- matrix(0, nrow = num_portfolios, ncol = num_assets)
colnames(portfolio_weights) <- tickers

# --- 5. Simulate random portfolios and calculate their statistics ---
for (i in 1:num_portfolios) {
  # Generate random weights that sum to 1.
  weights <- runif(num_assets)
  weights <- weights / sum(weights)
  
  # Store the weights.
  portfolio_weights[i, ] <- weights
  
  # Calculate portfolio return.
  # Annualize the monthly returns.
  annual_portfolio_return <- (sum(weights * mean_returns) + 1)^12 - 1
  
  # Calculate portfolio standard deviation.
  annual_portfolio_sd <- sqrt(t(weights) %*% (cov_matrix * 12) %*% weights)
  
  # Calculate Sharpe Ratio.
  annual_sharpe <- (annual_portfolio_return - risk_free_rate) / annual_portfolio_sd
  
  # Store the results.
  portfolio_returns[i] <- annual_portfolio_return
  portfolio_sd[i] <- annual_portfolio_sd
  portfolio_sharpe[i] <- annual_sharpe
}

# Combine the results into a data frame.
portfolio_data <- data.frame(
  Returns = portfolio_returns,
  SD = portfolio_sd,
  Sharpe = portfolio_sharpe
)

# --- 6. Find key portfolios on the Efficient Frontier ---
# Find the portfolio with the highest Sharpe Ratio (Tangency Portfolio).
max_sharpe_portfolio <- portfolio_data[which.max(portfolio_data$Sharpe), ]

# Find the portfolio with the minimum standard deviation (Minimum Variance Portfolio).
min_variance_portfolio <- portfolio_data[which.min(portfolio_data$SD), ]

# --- 7. Plot the Efficient Frontier and CML using ggplot2 ---
# Create the efficient frontier curve by filtering for the highest return for each level of risk.
efficient_frontier <- portfolio_data %>% 
  group_by(SD) %>% 
  summarise(Returns = max(Returns)) %>%
  ungroup()

# Generate the Capital Market Line (CML)
# The CML connects the risk-free rate to the tangency portfolio.
cml_slope <- (max_sharpe_portfolio$Returns - risk_free_rate) / max_sharpe_portfolio$SD
cml_intercept <- risk_free_rate

# Create a data frame for the CML line.
cml_data <- data.frame(
  SD = seq(0, max(portfolio_data$SD) * 1.1, length.out = 100),
  Returns = cml_intercept + cml_slope * seq(0, max(portfolio_data$SD) * 1.1, length.out = 100)
)

#Create the label for plotting
labeltangency = paste("Tangency Portfolio",sprintf("%.3f",max_sharpe_portfolio$SD),",",sprintf("%.3f",max_sharpe_portfolio$Returns))
labelminvar = paste("Minimum Variance",sprintf("%.3f",min_variance_portfolio$SD),",",sprintf("%.3f",min_variance_portfolio$Returns))

# Plotting using ggplot2
ggplot() +
  # Plot all simulated portfolios.
  geom_point(data = portfolio_data, aes(x = SD, y = Returns, color = Sharpe), alpha = 0.5) +
  # Plot the efficient frontier curve.
  geom_point(data = efficient_frontier, aes(x = SD, y = Returns), color = "black", size = 1.2) +
  # Plot the Capital Market Line.
  # Use a thicker dashed line to make it stand out.
  geom_line(data = cml_data, aes(x = SD, y = Returns), color = "red", size = 1, linetype = "dashed") +
  # Mark the key portfolios.
  geom_point(data = max_sharpe_portfolio, aes(x = SD, y = Returns), color = "red", size = 4, shape = 17, show.legend = FALSE) +
  geom_point(data = min_variance_portfolio, aes(x = SD, y = Returns), color = "blue", size = 4, shape = 18, show.legend = FALSE) +
  # Add a point for the risk-free asset.
  geom_point(aes(x = 0, y = risk_free_rate), color = "red", size = 4, shape = 15, show.legend = FALSE) +
  # Add text labels for the key points.
  geom_text(aes(x = max_sharpe_portfolio$SD + 0.01, y = max_sharpe_portfolio$Returns, label = labeltangency), hjust = 0, vjust = 0, size = 4, color = "red") +
  geom_text(aes(x = min_variance_portfolio$SD + 0.01, y = min_variance_portfolio$Returns, label = labelminvar), hjust = 0, vjust = 0, size = 4, color = "blue") +
  geom_text(aes(x = 0.01, y = risk_free_rate, label = "Risk-Free Asset"), hjust = 0, vjust = 0, size = 4, color = "red") +
  # Set the theme and labels.
  scale_color_gradient(low = "yellow", high = "red") +
  labs(
    title = "Efficient Frontier & Capital Market Line (CML)",
    subtitle = paste0("Assets: ", paste(tickers, collapse = ", "), "\nRisk-Free Rate: ", round(risk_free_rate * 100, 2), "%"),
    x = "Annualized Portfolio Standard Deviation",
    y = "Annualized Portfolio Return",
    color = "Sharpe Ratio"
  ) +
  theme_minimal()



```
