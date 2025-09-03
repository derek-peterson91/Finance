

# 1) Load source files for needed functions
source("R/paths.R")          # defines data_raw_dir(), data_clean_dir()
source("R/load_factors.R")   # defines load_ff_factors_daily()
source("R/load_prices.R")    # defines download_prices()
source("R/returns_utils.R")  # defines prices_to_returns(), align_with_factors()
source("R/estimate_betas.R") # defines estimate_rolling_betas
source("R/covariances.R")
source("R/risk_decomp.R")
source("R/report_plots.R")

# 2) Define investment universe
tickers <- c("SPY", # S&P 500, broad U.S. market
             "IWM", # Russell 2000 small caps
             "EFA", # Developed Markets ex-US
             "EEM", # Emerging Markets
             "LQD", # IG corporate bonds
             "HYG", # HY corporate bonds
             "TLT", # 20+ yr treasuries
             "DBC", # Commodity index
             "VNQ") # REITs

# 3) Download daily ticker prices, convert to daily percent returns 
#    and combine with factors
asset_prices <- download_prices(
  tickers      = tickers,
  start_date   = "2010-01-01",
  end_date     = Sys.Date()
)

# convert prices to percent returns
asset_returns <- prices_to_returns()            

# load factors
factors       <- load_ff_factors_daily()

# join returns and factors
aligned       <- align_with_factors(asset_returns, factors)


# 4) Estimate rolling betas
est <- estimate_rolling_betas(aligned, window = 252, min_obs = 180)

dplyr::glimpse(est$betas)
dplyr::glimpse(est$resid_var)


# 5) Define weights
# start with equal weight portfolio
w <- c(SPY = 0.1111, IWM = 0.1111, EFA = 0.1111, 
       EEM = 0.1111, LQD = 0.1111, HYG = 0.1111, 
       TLT = 0.1111, DBC = 0.1111, VNQ = 0.1111)

# Run the decomposition time series (uses same 252d / 180obs windows by default)
betas_csv <- file.path(data_clean_dir(), "rolling_betas.csv")
resid_csv <- file.path(data_clean_dir(), "rolling_resid_var.csv")

decomp_ts <- portfolio_risk_timeseries(
  betas_csv = betas_csv,
  resid_csv = resid_csv,
  factors_df = factors,
  weights = w,
  window = 252,
  min_obs = 180
)

# Inspect latest date
latest <- max(decomp_ts$date)
decomp_latest <- dplyr::filter(decomp_ts, date == latest) %>%
  arrange(share)
decomp_latest

annualized <- decomp_latest %>%
  mutate(sd_annual = sd * sqrt(252)) %>%
  mutate(pct_of_sd = sd / sd[factor == "TOTAL"])

annualized

##### Plots #####
plot_factor_share(decomp_ts)
latest_tbl <- export_latest_table(decomp_ts)
plot_vol_contrib_latest(decomp_ts)
plot_vol_contrib_pie(decomp_ts)



