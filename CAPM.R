library(tidyverse)
library(tidyquant)

symbols <- c("SBIN.NS","ICICIBANK.NS","TATAMOTORS.NS")
prices <- tq_get(x=symbols,
                 from="2020-01-01",
                 to="2023-08-01")

returns <- prices %>% group_by(symbol) %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'monthly',
               type = "log") %>%
  slice(-1) %>%
  ungroup() %>%
  set_names(c("asset","date","returns"))

symbols <- returns %>% distinct(asset) %>% pull()

weight <- c(0.25,0.25,0.5)
weight

portfolio <- tibble(symbols, weight)
portfolio


portfolio_returns <- returns %>%
  tq_portfolio(assets_col  = asset,
               returns_col = returns,
               weights = weight, 
               rebalance_on = "months",
               col_rename = "returns")
portfolio_returns




market_returns <- tq_get(x='^NSEI',
                         from="2020-01-01",
                         to="2023-08-01") %>%
  #Convert prices to monthly returns
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn,
               period     = "monthly",
               type       = "log",
               col_rename = "returns") %>%
  slice(-1)



Portfolio_market_returns <- left_join(market_returns,
                                          portfolio_returns,
                                          "date") %>% 
  set_names("date", "market_returns", "portfolio_returns")




Portfolio_market_returns %>%
  tq_performance(Ra = portfolio_returns,
                 Rb = market_returns,
                 performance_fun = CAPM.beta)





Portfolio_market_returns %>%
  ggplot(aes(x = market_returns, y = portfolio_returns)) +
  geom_point(color = "cornflowerblue") + 
  geom_smooth(method = "lm",
              se = FALSE,
              size = 1.5,
              color = tidyquant::palette_light()[3]) +
  labs(y = "portfolio returns",
       x = "market returns")



