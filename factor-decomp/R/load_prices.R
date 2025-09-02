

suppressPackageStartupMessages({
  library(tidyquant)
  library(readr)
  library(dplyr)
  library(lubridate)
  source("R/paths.R")
})


download_prices <- function(
    tickers = "SPY",
    start_date   = "2010-01-01",
    end_date     = Sys.Date(),
    out_file = file.path(data_raw_dir(), "ticker_prices.csv")
) {
  if (identical(tickers, "SPY")) warning("No ticker list provided. Defaulting to SPY only.")
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  prices <- tq_get(tickers, get = "stock.prices", from = start_date, to = end_date) %>%
    select(symbol, date, adjusted) %>%
    rename(ticker = symbol) %>%
    arrange(ticker, date) %>%
    distinct(ticker, date, .keep_all = TRUE)
  
  write_csv(prices, out_file)
  return(prices)
}

