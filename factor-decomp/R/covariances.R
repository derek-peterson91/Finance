
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
})

# Compute rolling factor covariance matrices (list-column of matrices)
# factors_df must have: date, mkt_rf, smb, hml, rmw, cma, mom

compute_factor_covariances <- function(factors_df, window = 252, min_obs = 180){
  fac_cols <- c("mkt_rf", "smb", "hml", "rmw", "cma", "mom")
  stopifnot(all(c('date', fac_cols) %in% names(factors_df)))
  
  f <- factors_df %>%
    arrange(date) %>%
    select(date, all_of(fac_cols))
  
  n <- nrow(f)
  
  out <- vector("list", n)
  j <- 0
  for (i in seq_len(n)){
    start_i <- max(1, i - window + 1)
    sl <- f[start_i:i, ]
    if (nrow(sl) < min_obs) next
    S <- stats::cov(as.matrix(sl[fac_cols]), use = 'pairwise.complete.obs')
    j <- j + 1
    out[[j]] <- tibble::tibble(date = sl$date[nrow(sl)], Sigma_f = list(S))
  }
  
  dplyr::bind_rows(out[seq_len(j)])
  
}

