library(tidyverse)
library(tidyquant)
library(dplyr)

symbols <- c("SBIN.NS",
             "ICICIBANK.NS",
             "TATAMOTORS.NS",
             "RELIANCE.NS",
             "TCS.NS",
             "HDFCBANK.NS",
             "INFY.NS",
             "HDFC.NS",
             "KOTAKBANK.NS",
             "LT.NS",
             "ITC.NS",
             "HINDUNILVR.NS",
             "HDFCLIFE.NS",
             "BAJFINANCE.NS",
             "AXISBANK.NS",
             "ASIANPAINT.NS",
             "MARUTI.NS",
             "M&M.NS",
             "ULTRACEMCO.NS",
             "TITAN.NS",
             "BHARTIARTL.NS",
             "BAJAJ-AUTO.NS",
             "HCLTECH.NS",
             "SHREECEM.NS",
             "WIPRO.NS",
             "INFY.NS",
             "IOC.NS",
             "HINDALCO.NS",
             "ONGC.NS",
             "POWERGRID.NS")
prices<- tq_get(x=symbols,
                get="stock.prices",
                from="2020-01-01",
                to="2023-06-01")

returns <- prices %>% group_by(symbol) %>% 
  tq_transmute(select=adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               type = "log")  %>%
  ungroup() %>%
  set_names("Asset","Date","Returns")

symbols <- returns %>% distinct(Asset) %>% pull()

# Set the seed for reproducibility
set.seed(123)

# Generate 30 random values between 0 and 1
weights <- runif(28)

# Normalize the values to make their sum equal to 1
weights <- weights / sum(weights)


w_tbl <- tibble(symbols, weights)
w_tbl


portfolio_returns <- returns %>%
  
  tq_portfolio(assets_col   = Asset, 
               returns_col  = Returns, 
               weights      = w_tbl, 
               rebalance_on = "months")

portfolio_returns

portfolio_sd<- portfolio_returns %>%
  
  tq_performance(Ra = portfolio.returns, 
                 performance_fun = table.Stats) %>%
  
  select(Stdev) %>%
  mutate(tq_sd = round(Stdev, 4))

portfolio_sd

portfolio_mean <- mean(portfolio_returns$portfolio.returns)

portfolio_mean

# Expected Returns vs Risk
sd_mean <- returns %>%
  
  group_by(Asset) %>%
  tq_performance(Ra = Returns, 
                 performance_fun = table.Stats) %>%
  select(Mean = ArithmeticMean, Stdev) %>%
  ungroup() %>%
  
  # Add Portfolio Standard Deviation
  add_row(tibble(Asset = "Portfolio",
                 Mean  = portfolio_mean, 
                 Stdev = portfolio_sd$tq_sd))

sd_mean



sd_mean%>%
  
  ggplot(aes(x = Stdev, y = Mean, color = Asset)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = Asset),size=3)

