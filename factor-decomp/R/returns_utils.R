
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

# prices_to_returns:
# Reads long prices (ticker, date, adjusted) and returns daily returns per ticker.
# price_file defaults to data_raw_dir()/ticker_prices.csv, defined in paths.R.
prices_to_returns <- function(price_file = file.path(data_raw_dir(), "ticker_prices.csv")) {
  prices <- read_csv(price_file, show_col_types = FALSE)
  stopifnot(all(c("ticker","date","adjusted") %in% names(prices)))
  
  prices %>%
    arrange(ticker, date) %>%
    group_by(ticker) %>%
    mutate(ret = adjusted / lag(adjusted) - 1) %>%
    ungroup() %>%
    filter(!is.na(ret))
}

# align_with_factors:
# Joins asset returns with factor data and computes excess = ret - rf.
# By default it loads factors via load_ff_factors_daily() so you don't have to pass them.
align_with_factors <- function(asset_returns,
                               factors = load_ff_factors_daily(),
                               out_file = file.path(data_clean_dir(), "asset_returns_with_factors.csv")) {
  dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
  
  needed <- c("date","mkt_rf","smb","hml","rmw","cma","rf","mom")
  stopifnot(all(needed %in% names(factors)))
  
  df <- asset_returns %>%
    inner_join(factors %>% select(all_of(needed)), by = "date") %>%
    mutate(excess = ret - rf) %>%
    arrange(ticker, date)
  
  readr::write_csv(df, out_file)
  df
}

