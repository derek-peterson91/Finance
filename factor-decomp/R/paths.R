suppressPackageStartupMessages({ library(here) })

# single source of truth for project directories
data_raw_dir  <- function(...) here::here("data_raw",  ...)
data_clean_dir<- function(...) here::here("data_clean",...)
reports_dir   <- function(...) here::here("reports",   ...)
