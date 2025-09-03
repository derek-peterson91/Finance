

source("R/paths.R")         # defines data_raw_dir(), data_clean_dir()
source("R/load_factors.R")  # defines load_ff_factors_daily()
source("R/load_prices.R")   # defines download_prices()
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

asset_prices <- download_prices(
  tickers      = tickers,
  start_date   = "2010-01-01",
  end_date     = Sys.Date()
)

asset_returns <- prices_to_returns()              # reads data_raw/ticker_prices.csv
factors       <- load_ff_factors_daily()
aligned       <- align_with_factors(asset_returns, factors)
