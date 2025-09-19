source("R/paths.R")
source("R/load_factors.R")
source("R/returns_utils.R")
source("R/estimate_betas.R")
source("R/covariances.R")
source("R/risk_decomp.R")
source("R/efficient_portfolio.R")

tickers <- c("SPY","IWM","EFA","EEM","LQD","HYG","TLT","DBC","VNQ")
factors <- load_ff_factors_daily()

# equal weights
w_ew <- setNames(rep(1/length(tickers), length(tickers)), tickers)

# optimal weights (from your CSV)
best <- readr::read_csv(file.path(data_clean_dir(), "efficient_weights_random.csv"), show_col_types = FALSE)
w_ow <- setNames(rep(0, length(tickers)), tickers); w_ow[best$ticker] <- best$weight

# build decomp_ts (heavy)
decomp_ew <- portfolio_risk_timeseries(
  file.path(data_clean_dir(),"rolling_betas.csv"),
  file.path(data_clean_dir(),"rolling_resid_var.csv"),
  factors, weights=w_ew, window=252, min_obs=180
)
decomp_ow <- portfolio_risk_timeseries(
  file.path(data_clean_dir(),"rolling_betas.csv"),
  file.path(data_clean_dir(),"rolling_resid_var.csv"),
  factors, weights=w_ow, window=252, min_obs=180
)

saveRDS(decomp_ew, file.path(reports_dir(), "decomp_ew.rds"))
saveRDS(decomp_ow, file.path(reports_dir(), "decomp_ow.rds"))
