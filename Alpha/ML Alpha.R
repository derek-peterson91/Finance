

set.seed(42)

library(data.table)
library(dplyr)
library(lubridate)
library(tidyquant)
library(slider)

configs <- list(
  horizon_days   = 21L,
  data_paths     = list(
    prices       = 'data/prices.csv',
    fundamentals = 'data/fundamentals.csv',
    macro        = 'data/macro.csv'
  ),
  
  schema = list(
    prices       = c("id", "date", "adj_close", "volume"),
    fundamentals = c("id", "publish_date", "pe", "pb", "ps", "div_yield"),
    macro        = c("publish_date", "cpi_yoy", "unemployment_rate", "term_spread", "fedFunds")
  )
)

check_schema <- function(dt, required_cols, name){
  missing <- setdiff(required_cols, names(dt))
  if(length(missing)) stop(sprintf("`%s` missing columns: %s", 
                                   name,
                                   paste(missing, collapse = ", ")),
                           call.=FALSE)
  invisible(TRUE)
}

load_all_data <- function(cfg){
  # --- Prices --- #
  prices <- fread(cfg$data_paths$prices)
  check_schema(prices, cfg$schema$prices, "prices")
  prices <- prices %>%
    mutate(date = as.Date(date))
  
  # --- Fundamentals --- #
  fundamentals <- fread(cfg$data_paths$fundamentals)
  check_schema(fundamentals, cfg$schema$fundamentals, "fundamentals")
  fundamentals <- fundamentals %>%
    mutate(publish_date = as.Date(publish_date))
  
  # --- Macro --- #
  macro <- fread(cfg$data_paths$macro)
  check_schema(macro, cfg$schema$macro, "macro")
  macro <- macro %>%
    mutate(publish_date = as.Date(publish_date))
  
  list(prices = prices, 
       fundamentals = fundamentals,
       macro = macro)
}

tickers <- c("NVDA", "MSFT", "AAPL", "AMZN", "META", "AVGO", "GOOG", "TSLA", "ORCL", "JPM")

prices <- tq_get(
  tickers, 
  from = '2019-01-01',
  to = '2025-08-31',
  get = "stock.prices"
)

prices <- prices %>%
  select(symbol, date, adjusted, volume) %>%
  rename(id = symbol, adj_close = adjusted)

##### Import fundamentals from calcbench #####
funds <- read_csv('C:\\Derek\\R\\Finance\\Alpha\\data\\calcbench.csv')

# convert eps, revenue, and dividends to trailing 4Q
funds %>%
  arrange(ticker, publish_date) %>%
  group_by(ticker) %>%
  mutate(EPS_TTM = slide_dbl(EPSDiluted, sum, .before = 3, .complete = TRUE),
         Rev_TTM = slide_dbl(Revenue, sum, .before = 3, .complete = TRUE),
         Div_TTM = slide_dbl(CommonStockDividendsPerShare, sum, .before = 3, .complete = TRUE)
  ) %>%
  ungroup()


h <- 21L
prices <- prices %>%
  arrange(id, date)

make_target <- function(pr, h = 21L) {
  setDT(pr); setkey(pr, id, date)
  pr[, logp := log(adj_close)]
  pr[, ret_fwd := shift(logp, -h) - logp, by = id]
  pr[!is.finite(ret_fwd), ret_fwd := NA_real_]
  pr[]
}

prices_t <- make_target(prices, h)
head(prices_t)
View(prices_t)
