
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

# function to read the csv files
load_ff_factors_daily <- function() {
  ff_path <- 'C:\\Derek\\R\\Finance\\factor-decomp\\data_raw'
  
  # Load the factors csv
  ff <-
    read_csv(file.path(ff_path, 'F-F_Research_Data_5_Factors_2x3_Daily.csv'),
             skip = 3) %>%
    janitor::clean_names() %>%
    rename(date = x1) %>%
    mutate(date = ymd(date), across(-date, ~ .x / 100)) %>%
    filter(!is.na(date))
  
  # Load the momentum csv
  mom <- 
    read_csv(file.path(ff_path, 'F-F_Momentum_Factor_Daily.csv'),
             skip = 12) %>%
    janitor::clean_names() %>%
    rename(date = x1) %>%
    mutate(date = ymd(date),
           across(-date, ~.x/100)) %>%
    filter(!is.na(date))
  
  # Join the 2 dataframes
  factors <- ff %>%
    inner_join(mom, by = 'date') %>%
    arrange(date)
  
  return(factors)
}





