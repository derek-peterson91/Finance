
suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(sandwich)  # Newey–West
  library(lmtest)    # coeftest
})


# Fit a regression in a single window
fit_one <- function(df){
  fit <- lm(excess ~ mkt_rf + smb + hml + rmw + cma + mom, data = df)
  vc <- NeweyWest(fit, prewhite = FALSE, adjust = TRUE)
  ct <- coeftest(fit, vcov. = vc)
  
  # Tidy betas + standard errors
  tb <- broom::tidy(ct) %>%
    select(term, estimate, std.error = `Std.Error`, statistic = `t value`, p.value = `Pr(>|t|)`)
  
  # Residual variance  + R²
  res_var <- var(residuals(fit), na.rm = TRUE)
  r2 <- summary(fit)$r.squared
  
  list(betas = tb, res_var = res_var, r2 = r2)
  
}


