

suppressPackageStartupMessages({
  library(tidyquant)
  library(readr)
  library(dplyr)
  library(lubridate)
})

download_prices <- function(tickers = 'SPY',
                            start = '2010-01-01',
                            end = Sys.Date(),
                            data_raw_dir = 'C:\\Derek\\R\\Finance\\factor-decomp\\data_raw') {
  
  # warn user if no tickers were given
  if (identical(tickers, "SPY")) {
    warning("No ticker list provided. Defaulting to SPY only.")
  }
  
  dir.create(data_raw_dir,
             showWarnings = FALSE,
             recursive = TRUE)
  
  prices <- tq_get(tickers,
                   get = 'stock.prices',
                   from = start,
                   to = end) %>%
    select(symbol, date, adjusted) %>%
    rename(ticker = symbol) %>%
    arrange(ticker, date)
  
  write_csv(prices, file.path(data_raw_dir, "ticker_prices.csv"))
  return(prices)
}

