
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

# Helper: wide betas (per ticker/date) -> matrix B aligned to a given date and set of tickers
# betas_tbl: cols ticker,date,mkt_rf,smb,hml,rmw,cma,mom

betas_matrix_for <- function(betas_tbl, date, tickers) {
  facs <- c('mkt_rf', 'smb', 'hml', 'rmw', 'cma', 'mom')
  B <- betas_tbl %>%
    filter(date == !!date, ticker %in% tickers) %>%
    arrange(match(ticker, tickers)) %>%
    select(all_of(facs)) %>%
    as.matrix()
  
  rownames(B) <- tickers
  colnames(B) <- facs
  B
}

# Helper: diagonal idiosyncratic variance matrix for tickers on a date
resid_diag_for <- function(resid_tbl, date, tickers){
  v <- resid_tbl %>%
    filter(date == !!date, ticker %in% tickers) %>%
    arrange(match(ticker, tickers)) %>%
    pull(resid_var)
  
  if (length(v) != length(tickers)) return(NULL)
  diag(v, nrow = length(tickers))
}

# Core portfolio risk decomposition for a single date
# returns a tibble with factor variance contributions, idiosyncratic variance, total var/sd
portfolio_risk_one_date <- function(
    weights,            # named numeric vector (names = tickers)
    B,                  # betas matrix (assets x factors)
    Sigma_f,            # factor covariance (factors x factors)
    Sigma_e             # idiosyncratic diag (assets x assets)
){
  # align weight order with B rows
  w <- matrix(as.numeric(weights[rownames(B)]), ncol = 1)
  V_assets <- B %*% Sigma_f %*% t(B) + Sigma_e
  var_p <- drop(t(w) %*% V_assets %*% w)
  sd_p  <- sqrt(var_p)
  
  # factor contributions (variance-level) via portfolio factor exposure
  b_p <- drop(t(weights[rownames(B)]) %*% B)
  
  # Each factor's variance contribution including covariances:
  contrib_mat <- (b_p %o% b_p) * Sigma_f
  factor_var_contrib <- colSums(contrib_mat)
  
  # idiosyncratic variance contribution
  idio_var <- drop(t(w) %*% Sigma_e %*% w)
  
  tibble::tibble(
    factor = c(colnames(Sigma_f), "idiosyncratic", "TOTAL"),
    var    = c(as.numeric(factor_var_contrib), idio_var, var_p),
    sd     = c(sqrt(pmax(factor_var_contrib, 0)), sqrt(max(idio_var, 0)), sd_p),
    share  = var / var_p
  )
}

# Convenience wrapper: load CSVs and produce a time series of decompositions
# betas_csv: data_clean/rolling_betas.csv
# resid_csv: data_clean/rolling_resid_var.csv
# factors_df: original factor returns (date + 6 factors, decimals)
# weights: named numeric vector with names matching tickers in betas_csv
# Returns a tibble with date, factor/idiosyncratic/TOTAL rows and var/sd/share per date

portfolio_risk_timeseries <- function(
    betas_csv,
    resid_csv,
    factors_df, 
    weights, 
    window = 252,
    min_obs = 180
){
  stopifnot(all(names(weights) != ""))
  
  betas_tbl <- readr::read_csv(betas_csv, show_col_types = FALSE)
  resid_tbl <- readr::read_csv(resid_csv, show_col_types = FALSE)
  
  # Compute rolling factor covariances on the fly
  Sigma_f_tbl <- compute_factor_covariances(factors_df, window = window, min_obs = min_obs)
  
  # Valid dates are intersections of (beta dates for all tickers in weights) and Sigma_f dates
  need_tickers <- names(weights)
  betas_dates <- betas_tbl %>%
    filter(ticker %in% need_tickers) %>%
    count(date) %>%
    filter(n == length(need_tickers)) %>%
    pull(date)
  
  dates <- intersect(betas_dates, Sigma_f_tbl$date) %>% sort()
  
  message("Starting portfolio risk decomposition over ", length(dates), " dates...")
  
  out <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    d <- dates[i]
    if (i %% 25 == 0 || i == length(dates)) {
      message("  Processing date ", i, " of ", length(dates), " (", d, ")")
    }

    B <- betas_matrix_for(betas_tbl, d, need_tickers)
    if (is.null(B) || anyNA(B)) next

    Sigma_e <- resid_diag_for(resid_tbl, d, need_tickers)
    if (is.null(Sigma_e)) next

    Sigma_f <- Sigma_f_tbl %>% filter(date == d) %>% pull(Sigma_f) %>% .[[1]]

    decomp <- portfolio_risk_one_date(weights, B, Sigma_f, Sigma_e) %>%
      mutate(date = d, .before = 1)
    out[[i]] <- decomp
  }

  message("Finished portfolio risk decomposition.")
  
  dplyr::bind_rows(out) %>%
    arrange(date, factor)
}
