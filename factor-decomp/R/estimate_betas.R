
suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(sandwich)  # Newey–West
  library(lmtest)    # coeftest
})


# robust tidy of coeftest() across broom versions
tidy_coefs <- function(ct) {
  tb <- broom::tidy(ct)
  nm <- names(tb)
  names(tb) <- gsub("[ .]+", "_", tolower(nm))
  tb %>%
    dplyr::select(term, estimate,
                  std_error = dplyr::any_of(c("std_error","std_error_")),
                  statistic,
                  p_value   = dplyr::any_of(c("p_value","p_value_")))
}

fit_one <- function(df) {
  fit <- lm(excess ~ mkt_rf + smb + hml + rmw + cma + mom, data = df)
  vc  <- NeweyWest(fit, prewhite = FALSE, adjust = TRUE)
  ct  <- coeftest(fit, vcov. = vc)
  tb  <- tidy_coefs(ct)
  res_var <- stats::var(stats::residuals(fit), na.rm = TRUE)
  r2      <- summary(fit)$r.squared
  list(betas = tb, res_var = res_var, r2 = r2)
}

estimate_rolling_betas <- function(
    aligned_df,
    window = 252,
    min_obs = 180,
    out_betas   = file.path(data_clean_dir(), "rolling_betas.csv"),
    out_resvars = file.path(data_clean_dir(), "rolling_resid_var.csv")
) {
  need <- c("ticker","date","excess","mkt_rf","smb","hml","rmw","cma","mom")
  stopifnot(all(need %in% names(aligned_df)))
  dir.create(dirname(out_betas), showWarnings = FALSE, recursive = TRUE)
  
  adf <- aligned_df %>% arrange(ticker, date)
  
  # split by ticker (returns a list of tibbles)
  by_ticker <- adf %>% group_split(ticker, .keep = TRUE)
  
  betas_list <- list()
  resv_list  <- list()
  
  for (d in by_ticker) {
    tk <- unique(d$ticker)
    n  <- nrow(d)
    
    message("Processing ticker: ", tk, " (", n, " rows)")
    
    if (n < min_obs) {
      message("  -> skipped (too few rows)")
      next
    }
    
    out_beta <- list()
    out_resv <- list()
    
    # loop over each rolling window
    for (i in seq_len(n)) {
      if (i %% 50 == 0) {
        message("    ... window ", i, " of ", n, " for ", tk)
      }
      
      start_i <- max(1, i - window + 1)
      sl <- d[start_i:i, ]
      if (nrow(sl) < min_obs) next
      
      fitres <- fit_one(sl)
      
      betas_row <- fitres$betas %>%
        dplyr::filter(term != "(Intercept)") %>%
        dplyr::select(term, estimate) %>%
        tidyr::pivot_wider(names_from = term, values_from = estimate) %>%
        dplyr::mutate(date = sl$date[nrow(sl)],
                      r2   = fitres$r2,
                      ticker = tk) %>%
        dplyr::select(ticker, date, r2, dplyr::everything())
      
      resv_row <- tibble::tibble(
        ticker = tk,
        date = sl$date[nrow(sl)],
        resid_var = fitres$res_var
      )
      
      out_beta[[length(out_beta)+1]] <- betas_row
      out_resv[[length(out_resv)+1]] <- resv_row
    }
    
    if (length(out_beta)) betas_list[[length(betas_list)+1]] <- dplyr::bind_rows(out_beta)
    if (length(out_resv)) resv_list[[length(resv_list)+1]]   <- dplyr::bind_rows(out_resv)
  }
  
  betas_all <- if (length(betas_list)) dplyr::bind_rows(betas_list) else dplyr::tibble()
  resv_all  <- if (length(resv_list))  dplyr::bind_rows(resv_list)  else dplyr::tibble()
  
  readr::write_csv(betas_all, out_betas)
  readr::write_csv(resv_all,  out_resvars)
  
  list(betas = betas_all, resid_var = resv_all)
}
