

source("R/paths.R")         # defines data_raw_dir(), data_clean_dir()
source("R/load_factors.R")  # defines load_ff_factors_daily()
source("R/returns_utils.R") # defines prices_to_returns(), align_with_factors()

asset_rets <- prices_to_returns()          # reads data_raw/ticker_prices.csv by default
aligned    <- align_with_factors(asset_rets)  # factors auto-loaded
dplyr::glimpse(aligned)
