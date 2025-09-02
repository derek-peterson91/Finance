

source("R/paths.R")         # defines data_raw_dir(), data_clean_dir()
source("R/load_factors.R")  # defines load_ff_factors_daily()
source("R/returns_utils.R") # defines prices_to_returns(), align_with_factors()

tickers <- c("SPY", # S&P 500, broad U.S. market
             "IWM", # Russell 2000 small caps
             "EFA", # Developed Markets ex-US
             "EEM", # Emerging Markets
             "LQD", # IG corporate bonds
             "HYG", # HY corporate bonds
             "TLT", # 20+ yr treasuries
             "DBC", # Commodity index
             "VNQ") # REITs

asset_prices <- download_prices(tickers)   
asset_returns <- prices_to_returns()

aligned    <- align_with_factors(asset_returns)  # factors auto-loaded
dplyr::glimpse(aligned)
