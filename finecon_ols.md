layout: default
---

## Basic Linear Regression

I create a cheatsheet about Ordinary Least Square (OLS) that could be accessed through this [link](https://github.com/iputusukma-book/iputusukma-book.github.io/blob/7ec61bd675f022b00402be47498db23185d58a53/ipsh_finecon_linear%20regression.pdf). In order to provide a go-through explanation about simple linear regression, I also create a spreadsheet explaining the entire process of linear regression (that is happen on the backend of your statistical software). This spreadsheet could be accessed through this [link](https://drive.google.com/uc?export=download&id=19tnfTfIQ-JDamw6QiuE-2pNgNY5k84dw).


## Simple Linear Regression: Example of CAPM Regression

To give a context on applying simple linear regression in financial economics, I provide a follow-through simulation of CAPM regression using R language as follows. The stock ticker and index ticker is discretionary and for this simulation I use `BBCA.JK` and `^JKSE` respectively.

### Installing and activate packages

```R
install.packages(c("quantmod", "dplyr", "ggplot2", "PerformanceAnalytics"))

library(quantmod)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
```

### Determine the stock, indices, and time-horizon that is going to be analyzed

```R
# Define tickers and date range
stock_ticker <- "BBCA.JK"
index_ticker <- "^JKSE"
start_date <- as.Date("2015-01-01")
end_date <- as.Date("2019-12-31")
#end_date <- Sys.Date()

```

### Retrieve the data using `getSymbols` command
```R
# Get data
getSymbols(stock_ticker, src = "yahoo", from = start_date, to = end_date) 
getSymbols(index_ticker, src = "yahoo", from = start_date, to = end_date)

# Extract adjusted closing prices
stock_prices <- Ad(BBCA.JK)
index_prices <- Ad(JKSE)
```

### Combine the data using `merge` command
```R
# Merge Data
data <- merge(stock_prices, index_prices, join = "inner")
colnames(data) <- c("Stock", "Index")
```

### Calculate daily returns and descriptive statistics 
```R
# Calculate daily returns
returns <- na.omit(ROC(data, type = "discrete"))
returns_df <- data.frame(Date = index(returns), coredata(returns)

# 1st and 2nd momentum statistics
summary_stats <- returns_df %>%
  summarise(
    Mean_Stock = mean(Stock), 
    SD_Stock = sd(Stock),
    Mean_Index = mean(Index),
    SD_Index = sd(Index),
    Correlation = cor(Stock, Index)
  )
print(summary_stats)
```

### Estimating beta parameters using `lm` command 
```R
# Linear regression: Stock ~ Index
beta_model <- lm(Stock ~ Index, data = returns_df)
summary(beta_model)
summary(beta_model)$coefficients

label <- coef(beta_model)["Index"]  # --- assign the label for plot title
```

### Plotting the scatter plot and best-fit line 
```R
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

## Linear Regression: Example of Day-of-the-week Effect in Stock Return

The day-of-the-week effect is a well-documented calendar anomaly in financial markets where stock returns systematically vary depending on the day of the week—contradicting the Efficient Market Hypothesis, which assumes returns should be random and unpredictable. Common patterns observed globally for this kind of anomaly are:
* Monday Effect: Returns tend to be lower or negative on Mondays. This has been attributed to investor pessimism over the weekend or delayed reaction to bad news.
* Friday Effect: Returns are often higher on Fridays, possibly due to investor optimism or portfolio adjustments before the weekend.
* Midweek Neutrality: Tuesday through Thursday often show less pronounced patterns, though some markets exhibit midweek volatility.

### Installing and activate packages

```R
# If you haven't install the following packages, run this command
install.packages("quantmod")
install.packages("dplyr")
install.packages("lubridate")

library(quantmod)
library(dplyr)
library(lubridate)
```

### Retrieve stock indices from Yahoo Finance (ticker: ^JKSE)

```R
# The `getSymbols` function stores the data in an object named after the ticker.
# In this analysis our time-horizon are the most rcent 5 years period based on the system date.
getSymbols("^JKSE", from = Sys.Date() - years(5), to = Sys.Date(), src = "yahoo")

indices_data <- JKSE
head(indices_data)
```
![img](/assets/img/indices.jpg)

### Calculate stock return and omit the NA value
```R
# The `periodReturn` function from quantmod is perfect for this.
# We'll calculate daily log returns based on the adjusted closing price.
returns <- dailyReturn(stock_data$JKSE.Adjusted, type = "log")

# The first value is usually NA because there is no prior day's price to calculate
# We  should remove this first value using this following syntax
returns <- na.omit(returns)

# Show the data
returns
```
![img](/assets/img/returns.jpg)

### Create day-of-the-week dummy variables using `lubridate::wday` and combine the data
```R
# We first extract the day of the week from the index (the date).
day_of_week <- wday(index(returns), label = TRUE)

# Combine the returns and the day variable into a single data frame.
df <- data.frame(
  Returns = as.numeric(returns),
  Day = day_of_week
)

# Now, create a binary dummy variable for each day of the week.
# We will use Monday as the reference category and omit its dummy variable to avoid multicollinearity in the regression.
df <- df %>%
  mutate(
    is_Tuesday = as.numeric(Day == "Tue"),
    is_Wednesday = as.numeric(Day == "Wed"),
    is_Thursday = as.numeric(Day == "Thu"),
    is_Friday = as.numeric(Day == "Fri")
  )

tail(df)
```
![img](/assets/img/withdummy.jpg)

### Regress the stock return with day dummy variables as regressors and present the result
```R
# The `lm` function for linear regression will use our newly created dummy variables.
regression_model <- lm(Returns ~ is_Tuesday + is_Wednesday + is_Thursday + is_Friday, data = df)

# The `summary()` function provides a detailed output of the regression results, including coefficients, standard errors, t-statistics, p-values, and R-squared.
summary(regression_model)
```
![img](/assets/img/regress.jpg)


### Regress the stock return with day dummy variables as regressors and present the result

* Our hypothesis for this case study is that Monday return is negative:
   - $H_0$ : Monday Return $>=$0
   - $H_a$ : Monday Return $<$ 0

* Our second hypothesis is that Friday return is positive:
   - $H_0$ : Friday Return $<=$0
   - $H_a$ : Friday Return $>$ 0

The regression result could be interpreted as follows:
| Variable     | Estimate (p-value)| Interpretation |
| :---         |           ---: | :---           |
| Intercept    | -0.0001974 (0.736)   | Average return on Monday |
| is_Tuesday   | 0.0001395 (0.865)    | Tuesday return is 0.0001395 higher than Monday |
| is_Wednesday | 0.0013089 ( 0.114)   | Wednesday return is 0.0013089 higher than Monday |
| is_Thursday  | 0.0010744 (0.196)   | Thursday return is 0.0010744 higher than Monday |
| is_Friday    | 0.0002241 (0.788)    | Friday return is 0.0002241 higher than Monday |

* Albeit one of the coefficients are statistically significant (all p-values > 0.05), meaning we cannot confidently say these differences are real rather than random noise. Moreover, we fail to reject H₀. 
* The Monday return is negative in point estimate at -0.0001974, but not statistically significant with p-value 0.736. So, there's no strong evidence that Monday return is truly negative. 
* Furthermmore, we also fail to reject H₀ for Friday return. The Friday return is positive in estimate at 0.0002241, but not statistically significant with p-value 0.788. So, there's no strong evidence that Friday return is truly positive.




