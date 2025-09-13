---
layout: default
---

## Basic Linear Regression
------

I create a cheatsheet about Ordinary Least Square (OLS) that could be accessed through this [link](https://github.com/iputusukma-book/iputusukma-book.github.io/blob/7ec61bd675f022b00402be47498db23185d58a53/ipsh_finecon_linear%20regression.pdf). In order to provide a go-through explanation about simple linear regression, I create a spreadsheet explaining the entire process of linear regression (that is happen on the backend of your statistical software). This spreadsheet could be accessed through this [link](https://drive.google.com/uc?export=download&id=19tnfTfIQ-JDamw6QiuE-2pNgNY5k84dw).

The beta regression using R are as follows:
![CAPMReg](/assets/img/capmreg.jpg)

```R
install.packages(c("quantmod", "dplyr", "ggplot2", "PerformanceAnalytics"))

library(quantmod)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)

# Define tickers and date range
stock_ticker <- "BBCA.JK"
index_ticker <- "^JKSE"
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2019-12-31")
#end_date <- Sys.Date()

# Get data
getSymbols(stock_ticker, src = "yahoo", from = start_date, to = end_date) 
getSymbols(index_ticker, src = "yahoo", from = start_date, to = end_date)

# Extract adjusted closing prices
stock_prices <- Ad(BBCA.JK)
index_prices <- Ad(JKSE)

View(BBCA.JK)
View(JKSE)

View(stock_prices)
View(index_prices)


# Merge Data
data <- merge(stock_prices, index_prices, join = "inner")
colnames(data) <- c("Stock", "Index")

# Calculate daily returns
returns <- na.omit(ROC(data, type = "discrete"))
returns_df <- data.frame(Date = index(returns), coredata(returns)


summary_stats <- returns_df %>%
  summarise(
    Mean_Stock = mean(Stock), 
    SD_Stock = sd(Stock),
    Mean_Index = mean(Index),
    SD_Index = sd(Index),
    Correlation = cor(Stock, Index)
  )
print(summary_stats)

# Linear regression: Stock ~ Index
beta_model <- lm(Stock ~ Index, data = returns_df)
summary(beta_model)
summary(beta_model)$coefficients
label <- coef(beta_model)["Index"]


# Time series plot
ggplot(returns_df, aes(x = Date)) +
  geom_line(aes(y = Stock, color = "Stock")) +
  geom_line(aes(y = Index, color = "Index")) +
  labs(title = "Daily Returns: Stock vs Index", y = "Return", color = "Legend") +
  theme_minimal()

# Scatter plot for beta
ggplot(returns_df, aes(x = Index, y = Stock)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("Beta Estimation: ",sprintf("%.3f",label)), x = "Index Return", y = "Stock Return") +
  theme_minimal()

```
