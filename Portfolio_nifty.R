library(quantmod)
library(dplyr)
library(tidyr)
library(PerformanceAnalytics)
library(quantmod)
library(ggplot2)
library(plotly)
library(tidyquant)
library(timetk)
library(tidyverse)
# Define the list of stock tickers
tick <- c('RELIANCE.NS', 'SBIN.NS', 'ICICIBANK.NS', 'CIPLA.NS', 'INFY.NS',
          'M&M.NS', 'DRREDDY.NS', 'TATAMOTORS.NS', 'BHEL.NS', 'TCS.NS',
          'HCLTECH.NS', 'HEROMOTOCO.NS', 'HINDALCO.NS', 'HINDUNILVR.NS',
          'BAJAJ-AUTO.NS', 'BPCL.NS', 'COALINDIA.NS', 'JSWSTEEL.NS', 'KOTAKBANK.NS',
          'LT.NS', 'MARUTI.NS', 'NTPC.NS', 'ONGC.NS', 'POWERGRID.NS', 'SUNPHARMA.NS',
          'TITAN.NS', 'UPL.NS', 'ULTRACEMCO.NS', 'WIPRO.NS', 'ADANIPORTS.NS',
          'ASIANPAINT.NS', 'AXISBANK.NS', 'BAJAJFINSV.NS', 'BAJFINANCE.NS',
          'BHARTIARTL.NS', 'GRASIM.NS', 'HDFCBANK.NS', 'INDUSINDBK.NS')

# Get historical price data for all stocks
price_data <- tq_get(tick,
                     from = '2014-01-01',
                     to = '2020-12-20',
                     get = 'stock.prices')

# Calculate daily log returns for all stocks
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')

# Convert log returns to xts format
log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# Calculate mean returns for all stocks
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

# Calculate covariance matrix
cov_mat <- cov(log_ret_xts) * 252
print(round(cov_mat, 4))

# Generate random portfolio weights
set.seed(123)  # For reproducibility
wts <- runif(n = length(tick))
print(wts)

# Normalize portfolio weights
wts <- wts / sum(wts)
print(wts)

# Calculate portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

# Calculate Sharpe ratio
sharpe_ratio <- port_returns / port_risk
print(sharpe_ratio)

# Number of portfolios to simulate
num_port <- 5000

# Creating matrices and vectors to store portfolio metrics
all_wts <- matrix(nrow = num_port, ncol = length(tick))
port_returns <- vector('numeric', length = num_port)
port_risk <- vector('numeric', length = num_port)
sharpe_ratio <- vector('numeric', length = num_port)

# Simulate random portfolios
for (i in seq_along(port_returns)) {
  wts <- runif(length(tick))
  wts <- wts / sum(wts)
  all_wts[i, ] <- wts
  
  # Portfolio returns
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  port_returns[i] <- port_ret
  
  # Portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat %*% wts))
  port_risk[i] <- port_sd
  
  # Portfolio Sharpe Ratios (assuming 0% Risk-free rate)
  sr <- port_ret / port_sd
  sharpe_ratio[i] <- sr
}

# Store portfolio metrics in a tibble
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

# Convert matrix to tibble and change column names
all_wts <- tk_tbl(all_wts)
colnames(all_wts) <- colnames(log_ret_xts)

# Combine portfolio metrics with weights
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

# Find minimum variance and maximum Sharpe ratio portfolios
min_var <- portfolio_values[which.min(portfolio_values$Risk), ]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio), ]

# Plot minimum variance portfolio weights
p_min_var <- min_var %>%
  gather(RELIANCE.NS:INDUSINDBK.NS, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset, Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

# Plot maximum Sharpe ratio portfolio weights
p_max_sr <- max_sr %>%
  gather(RELIANCE.NS:INDUSINDBK.NS, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset, Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent)

# Plot efficient frontier
p_eff_frontier <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  ylim(0, 0.20) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk, y = Return), data = min_var, color = 'red') +
  geom_point(aes(x = Risk, y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.20, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.18, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.14, xend = 0.135, y = 0.01, 
           yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.22, xend = 0.2275, y = 0.405, 
           yend = 0.365, color = 'red', arrow = arrow(type = "open"))

# Convert ggplot plots to plotly for interactivity
p_min_var_plotly <- ggplotly(p_min_var)
p_max_sr_plotly <- ggplotly(p_max_sr)
p_eff_frontier_plotly <- ggplotly(p_eff_frontier)

# Display the interactive plots
p_min_var_plotly
p_max_sr_plotly
p_eff_frontier_plotly

