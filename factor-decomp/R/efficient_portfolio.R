
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

# Annualize daily returns
annualize_stats <- function(ret_mat) {
  mu_d <- colMeans(ret_mat, na.rm = TRUE)
  mu_a <- mu_d * 252
  Sigma <- cov(ret_mat, use = 'pairwise')
  list(mu_annual = mu_a, Sigma_daily = Sigma)
}

# Create N long-only random weight portfolios
random_weights <- function(tickers, n_port = 20000, w_max = 1.0, seed = 1, batch = NULL, max_tries = 50) {
  set.seed(seed)
  K <- length(tickers)
  if (is.null(batch)) batch <- max(1000L, ceiling(n_port / 10))
  collected <- list()
  total <- 0L
  tries <- 0L
  
  need_cap <- !is.null(w_max) && w_max < 1
  
  while (total < n_port && tries < max_tries) {
    tries <- tries + 1
    # sample a batch via Dirichlet(1) ~ normalized exponential
    Wb <- matrix(rexp(batch * K, rate = 1), nrow = batch, ncol = K)
    Wb <- Wb / rowSums(Wb)
    
    if (need_cap) {
      keep <- apply(Wb, 1L, function(w) all(w <= w_max + 1e-12))
      if (any(keep)) Wb <- Wb[keep, , drop = FALSE] else Wb <- matrix(numeric(0), ncol = K)
    }
    
    if (nrow(Wb) > 0L) {
      collected[[length(collected) + 1L]] <- Wb
      total <- total + nrow(Wb)
    }
  }
  
  if (total == 0L) {
    stop("random_weights: no feasible portfolios found under w_max = ", w_max,
         " after ", tries, " tries. Consider relaxing the cap or increasing batch size.")
  }
  
  W <- do.call(rbind, collected)
  if (nrow(W) > n_port) W <- W[seq_len(n_port), , drop = FALSE]
  
  # ensure matrix + names
  W <- as.matrix(W)
  storage.mode(W) <- "double"
  colnames(W) <- tickers
  W
}

# Compute annualized Sharpe for each weight row given annualized mu and daily Sigma
# rf_annual is annualized risk-free (eg. from FF 'rf' * 252), default  = 0
sharpe_for_weights <- function(W, mu_annual, Sigma_daily, rf_annual = 0){
  # portfolio annualized mean return:
  mu_p <- as.numeric(W %*% mu_annual)
  # portfolio daily variance:
  var_p_d <- rowSums((W %*% Sigma_daily) * W)
  # annualized vol
  sd_p_a <- sqrt(var_p_d * 252)
  (mu_p - rf_annual) / sd_p_a
}

# Build a returns matrix aligned across selected tickers and dates
# Uses saved long prices CSV created by download_prices
load_returns_matrix <- function(price_file = file.path(data_raw_dir(), "ticker_prices.csv"),
                                tickers = NULL) {
  prices <- read_csv(price_file, show_col_types = FALSE) %>%
   select(ticker, date, adjusted) %>%
   arrange(ticker, date) %>%
   group_by(ticker) %>%
   mutate(ret = adjusted/lag(adjusted) - 1) %>%
   ungroup() %>%
   filter(!is.na(ret))
  
  if (!is.null(tickers)) {
    prices <-filter(prices, ticker %in% tickers)
  }
  
  # If nothing is left, stop early
  if (nrow(prices) == 0L) {
    stop("No return rows found for the requested tickers. Check your price file and tickers.")
  }
  
  wide <- tidyr::pivot_wider(prices, id_cols = date, names_from = ticker, values_from = ret) %>%
   arrange(date)
  
  dates <- wide$date
  ret_df <-select(wide, -date)
  
  # Coerce to matrix safely even if there's just one column
  ret_mat <- as.matrix(ret_df)
  if (is.null(dim(ret_mat))) {
    # single-column vector case: make it a 1-column matrix
    ret_mat <- matrix(ret_mat, ncol = 1)
  }
  
  # Set column names from the data frame’s names
  cn <- names(ret_df)
  if (length(cn) == 0L) {
    stop("After pivoting, there are 0 return columns. Do your tickers exist in the price file?")
  }
  colnames(ret_mat) <- cn
  
  # Ensure numeric
  storage.mode(ret_mat) <- "double"
  
  # Diagnostics
  message("load_returns_matrix: ", ncol(ret_mat), " tickers, ", nrow(ret_mat), " rows (pre-filter)")
  
  list(dates = dates, ret_mat = ret_mat)
}

# Find the max-Sharpe portfolio by random search
# window_days: look-back period for stats
# rf_from_factors: if provided from FF, we annualize and use as rf
find_max_sharpe_random <- function(tickers,
                                   price_file = file.path(data_raw_dir(), "ticker_prices.csv"),
                                   window_days = 252,
                                   n_port = 20000,
                                   w_max = 1.0,
                                   rf_from_factors = NULL,
                                   seed = 1,
                                   save_csv = file.path(data_clean_dir(), "efficient_weights_random.csv")) {
  dat      <- load_returns_matrix(price_file, tickers)
  dates    <- dat$dates
  ret_mat  <- dat$ret_mat
  stopifnot(is.matrix(ret_mat), is.numeric(ret_mat))
  
  # Drop rows with any NA across the selected tickers
  ok        <- stats::complete.cases(ret_mat)
  ret_clean <- ret_mat[ok, , drop = FALSE]
  dates_ok  <- dates[ok]
  
  if (ncol(ret_clean) < 2L) {
    stop("Only ", ncol(ret_clean), " ticker column remains after filtering complete cases. ",
         "Need >= 2 to form a portfolio. Check which tickers have data.")
  }
  if (nrow(ret_clean) < window_days) {
    stop("Not enough rows (", nrow(ret_clean), ") for window_days = ", window_days, ".")
  }
  
  # Take the last window_days rows
  ret_win <- ret_clean[(nrow(ret_clean)-window_days+1):nrow(ret_clean), , drop = FALSE]
  message("Stats window: ", window_days, " rows, ", ncol(ret_win), " tickers. Window end: ", as.character(tail(dates_ok, 1)))
  
  # Optional risk-free (annualized)
  rf_a <- 0
  if (!is.null(rf_from_factors)) {
    rf_slice <- tail(rf_from_factors[rf_from_factors$date %in% dates_ok, , drop = FALSE], window_days)
    if (nrow(rf_slice) > 0) rf_a <- mean(rf_slice$rf, na.rm = TRUE) * 252
  }
  
  mu_d   <- colMeans(ret_win, na.rm = TRUE)
  mu_a   <- mu_d * 252
  Sigma_d <- stats::cov(ret_win, use = "pairwise.complete.obs")
  
  message("Sampling ", n_port, " random portfolios...")
  W <- random_weights(colnames(ret_win), n_port = n_port, w_max = w_max, seed = seed)
  if (!is.matrix(W) || nrow(W) == 0L) {
    stop("No feasible weights sampled. Try a higher w_max (e.g., 0.45) or larger n_port.")
  }
  
  mu_p   <- as.numeric(W %*% mu_a)
  var_pd <- rowSums((W %*% Sigma_d) * W)
  sd_pa  <- sqrt(pmax(var_pd, 0) * 252)
  shp    <- (mu_p - rf_a) / sd_pa
  
  best_i <- which.max(shp)
  w_best <- W[best_i, , drop = TRUE]
  res <- tibble::tibble(ticker = names(w_best), weight = as.numeric(w_best)) %>%
    arrange(desc(weight))
  
  write_csv(res, save_csv)
  message("Saved: ", save_csv,
          " | Max Sharpe = ", round(max(shp, na.rm = TRUE), 3),
          " | Window end = ", as.character(tail(dates_ok, 1)))
  
  list(weights = res, sharpe = max(shp, na.rm = TRUE), rf_annual = rf_a,
       mu_annual = mu_a, Sigma_daily = Sigma_d, window_end = tail(dates_ok, 1))
}


