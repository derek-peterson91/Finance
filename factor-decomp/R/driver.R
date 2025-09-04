

# 1) Load source files for needed functions
source("R/paths.R")          # defines data_raw_dir(), data_clean_dir()
source("R/load_factors.R")   # defines load_ff_factors_daily()
source("R/load_prices.R")    # defines download_prices()
source("R/returns_utils.R")  # defines prices_to_returns(), align_with_factors()
source("R/estimate_betas.R") # defines estimate_rolling_betas
source("R/covariances.R")
source("R/risk_decomp.R")
source("R/report_plots.R")
source("R/efficient_portfolio.R")

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

equal_w <-
  setNames(rep(1 / length(tickers), length(tickers)), tickers)


# Run the decomposition time series (uses same 252d / 180obs windows by default)
betas_csv <- file.path(data_clean_dir(), "rolling_betas.csv")
resid_csv <- file.path(data_clean_dir(), "rolling_resid_var.csv")

decomp_ts <- portfolio_risk_timeseries(
  betas_csv = betas_csv,
  resid_csv = resid_csv,
  factors_df = factors,
  weights = equal_w,
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


##### OPTIMAL PORTFOLIO #####
best <- find_max_sharpe_random(
  tickers         = tickers,
  window_days     = 252,
  n_port          = 50000,
  w_max           = 0.35,
  rf_from_factors = dplyr::select(factors, date, rf),
  seed            = 42
)
best$weights

opt_w <- setNames(best$weights$weight, best$weights$ticker)

decomp_ts <- portfolio_risk_timeseries(
  betas_csv  = file.path(data_clean_dir(), "rolling_betas.csv"),
  resid_csv  = file.path(data_clean_dir(), "rolling_resid_var.csv"),
  factors_df = factors,
  weights    = opt_w,
  window     = 252,
  min_obs    = 180
)

# plots
plot_factor_share(decomp_ts)
plot_vol_contrib_latest(decomp_ts)
plot_vol_contrib_pie(decomp_ts)

################## COMPARISON################## 
factors <- load_ff_factors_daily()

decomp_ew <- portfolio_risk_timeseries(
  betas_csv  = file.path(data_clean_dir(), "rolling_betas.csv"),
  resid_csv  = file.path(data_clean_dir(), "rolling_resid_var.csv"),
  factors_df = factors,
  weights    = equal_w,
  window     = 252,
  min_obs    = 180
)

decomp_ow <- portfolio_risk_timeseries(
  betas_csv  = file.path(data_clean_dir(), "rolling_betas.csv"),
  resid_csv  = file.path(data_clean_dir(), "rolling_resid_var.csv"),
  factors_df = factors,
  weights    = opt_w,
  window     = 252,
  min_obs    = 180
)

betas_csv <- file.path(data_clean_dir(), "rolling_betas.csv")

# factors in order
fac_cols <- c("mkt_rf","smb","hml","rmw","cma","mom")

# 1) Load rolling betas by TICKER, then pick a common latest date
betas <- read_csv(betas_csv, show_col_types = FALSE) %>%
  select(ticker, date, all_of(fac_cols)) %>%
  arrange(date)

# choose the latest date where ALL your tickers have betas
tickers <- names(equal_w)  # or define same vector you used earlier
latest_common <- betas %>% filter(ticker %in% tickers) %>%
  count(date) %>% filter(n == length(tickers)) %>%
  summarise(d = max(date)) %>% pull(d)

B <- betas %>% filter(date == latest_common, ticker %in% tickers) %>%
  arrange(match(ticker, tickers)) %>%
  select(all_of(fac_cols)) %>% as.matrix()
rownames(B) <- tickers

# 2) Build weight vectors (named, same order as B’s rows)
w_ew <- rep(1/length(tickers), length(tickers)); names(w_ew) <- tickers
w_ow <- opt_w[tickers]  # from best$weights; ensure exact same ticker order

# 3) Portfolio betas = weighted average exposures: b_p = w' * B
beta_ew <- as.numeric(t(w_ew) %*% B); names(beta_ew) <- fac_cols
beta_ow <- as.numeric(t(w_ow) %*% B); names(beta_ow) <- fac_cols

# 4) Tidy comparison table
beta_compare <- bind_rows(
  tibble(portfolio = "Equal Weight",    factor = fac_cols, beta = beta_ew),
  tibble(portfolio = "Optimal Weight",  factor = fac_cols, beta = beta_ow)
) %>% tidyr::pivot_wider(names_from = portfolio, values_from = beta)

beta_compare

latest_date <- min(max(decomp_ew$date), max(decomp_ow$date))  # align dates

varshare_compare <- bind_rows(
  decomp_ew %>% filter(date == latest_date, factor != "TOTAL") %>%
    mutate(portfolio = "Equal Weight"),
  decomp_ow %>% filter(date == latest_date, factor != "TOTAL") %>%
    mutate(portfolio = "Optimal Weight")
) %>%
  # clamp & renormalize to avoid negatives/ >100% from estimation noise
  group_by(portfolio, date) %>%
  mutate(share = pmax(share, 0), share = share / sum(share)) %>%
  ungroup() %>%
  select(portfolio, factor, share) %>%
  tidyr::pivot_wider(names_from = portfolio, values_from = share)

varshare_compare























