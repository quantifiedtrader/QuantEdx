library(tidyquant)
library(tidyverse)


symbols <- c("CIPLA.NS", "MARUTI.NS", "SBIN.NS","RELIANCE.NS")

prices <- tq_get(x=symbols,from='2017-01-01',to='2023-08-01')

returns <- prices %>% group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period='monthly',
               type='log') %>%
  slice(-1) %>%
  ungroup() %>%
  set_names(c("asset","date","returns"))

# symbols
symbols <-returns %>% distinct(asset) %>% pull()
symbols

# weights
weights <- c(0.25, 0.25, 0.2, 0.3)
weights

portfolio <- tibble(symbols, weights)
portfolio


portfolio_returns <- returns %>%
  
  tq_portfolio(assets_col   = asset, 
               returns_col  = returns, 
               weights      = portfolio, 
               rebalance_on = "months", 
               col_rename   = "returns")

portfolio_returns



portfolio_kurtosis <- portfolio_returns %>%
  
  tq_performance(Ra = returns, 
                 performance_fun = table.Stats) %>%
  
  select(Kurtosis) 

portfolio_kurtosis


# Assign a value for window
window = 6

# Transform data: calculate 24 month rolling kurtosis
rolling_kurtosis <- portfolio_returns %>%
  
  tq_mutate(select     = returns, 
            mutate_fun = rollapply,
            width      = window,
            FUN        = kurtosis, 
            col_rename = "kurt") %>%
  
  na.omit() %>%
  select(-returns)

rolling_kurtosis


rolling_kurtosis %>%
  
  ggplot(aes(x = date, y = kurt)) +
  geom_line(color = "cornflowerblue") +
  
  # Formatting
  scale_y_continuous(breaks = seq(-1, 6, 0.5)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 7)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  
  #Labeling
  labs(x = NULL, 
       y = "Kurtosis",
       title = paste0("Rolling ", window, " Month Kurtosis")) +
  
  annotate(geom = "text", x = as.Date("2021-07-31"), y = 0.7,
           size = 5, color = "red",
           label = str_glue("Downside risk plummeted 
                              in the last quarter of 2020"))




# Data transformation: calculate skewness
asset_skewness <- returns %>%
  
  group_by(asset) %>%
  summarise(skew = skewness(returns)) %>%
  ungroup() %>%
  
  # Add portfolio skewness
  add_row(tibble(asset = "Portfolio",
                 skew  = skewness(portfolio_returns$returns)))

asset_skewness


# Plot skewness
asset_skewness %>%
  
  ggplot(aes(x = asset, y = skew, color = asset)) +
  geom_point(size=5) +
  
  ggrepel::geom_text_repel(aes(label = asset),
                           data = asset_skewness %>%
                             filter(asset == "Portfolio")) +
  
  labs(y = "skewness")


# Plot distribution of returns
returns %>%
  
  ggplot(aes(x = returns)) +
  geom_density(aes(color = asset), show.legend = FALSE, alpha = 1) +
  geom_histogram(aes(fill = asset), show.legend = FALSE, alpha = 0.3, binwidth = 0.01) +
  facet_wrap(~asset, ncol = 1, scales = "free_y") +
  
  # Labeling
  labs(title = "Distribution of Monthly Returns, 2017-2022",
       y = "Frequency",
       x = "Rate of Returns")



