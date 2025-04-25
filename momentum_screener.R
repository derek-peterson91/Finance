
library(tidyverse)
library(tidyquant)
library(httr)
library(rvest)
library(lubridate)

#---------- CONFIG BREAKOUT CRITETIA ----------#

soft_breakout_pct <- 0.005     # Within 0.5% of the 52-week high = breakout
proximity_threshold <- 0.05    # Within 5% of the 52-week high = near breakout
volume_threshold <- 1.2        # Current volume must be at least 1.2x of avg volume
lookback_days <- 365           # 1 year


#---------- UNIVERSE SELECTION ----------#

cat("\n=== Universe Options ===")
print('1 - SPY (S&P 500)')
print('2 - S&P1500')
print('3 - Russell 1000 (CSV Required)')
print('4 - Russell 3000 (CSV Required)')
print('5 - TSX Composite')

choice = readline("Select Universe [1/2/3/4/5]: ")

if(choice == 1) {
  universe_name <- "SPY"
  
  df <-
    read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)
  
  universe <- list(df$Symbol)
  
  
} else if (choice == 2) {
  universe_name <- 'S&P1500'
  
  df1 <-
    read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies") %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)
  df2 <-
    read_html("https://en.wikipedia.org/wiki/List_of_S%26P_400_companies") %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)
  df3 <-
    read_html("https://en.wikipedia.org/wiki/List_of_S%26P_600_companies") %>%
    html_nodes("table") %>%
    .[[1]] %>%
    html_table(fill = TRUE)
  
  universe <- list(c(df1$Symbol, df2$Symbol, df3$Symbol))
  
} else if (choice == 3) {
  universe_name <- 'Russell 1000'
  universe <- list(read_csv(file.choose())$Symbol)
  
} else if (choice == 4) {
  universe_name <- 'Russell 3000'
  universe <- list(read_csv(file.choose())$Symbol)
  
} else if (choice == 5) {
  df <-
    read_html("https://en.wikipedia.org/wiki/S%26P/TSX_Composite_Index") %>%
    html_nodes("table") %>%
    .[[4]] %>%
    html_table(fill = TRUE) %>%
    rename(Symbol = Ticker)
  
  universe <- list(df$Symbol)
  
} else {
  stop('Invalid Selection: Enter a numeric option from one of the following:\n
  1 - SPY (S&P 500) \n
  2 - S&P1500 \n
  3 - Russell 1000 \n
  4 - Russell 3000 \n
  5 - TSX Composite')
  
}

# Replace any dots in symbols (for TSX and some U.S. tickers)
universe <- sub("\\.", "-", universe[[1]])

cat(paste0("\n Universe Loaded: ",
           universe_name, " - ",
           length(universe), " symbols"))


#---------- DOWNLOAD DATA ---------- #


# Define start and end dates
end_date <- Sys.Date()
start_date <- end_date - lookback_days

# Download the stock data
tidy_stocks <- universe %>%
  tq_get(from = start_date,
         to = end_date)

# Add 50 day moving average and 52-high columns
close_prices <-
  tidy_stocks %>%
  select(symbol, date, volume, adjusted) %>%
  group_by(symbol) %>%
  mutate(avg_vol_50 = rollapply(volume, 50, mean, align = 'right', fill = NA)) %>%
  mutate(rolling_high = cummax(adjusted))

# Extract dates for joining later
dates <- close_prices %>%
  group_by(symbol) %>%
  slice_tail() %>%
  select(symbol, date)

# Extract last closing price for all stocks
current_close <- close_prices %>%
  group_by(symbol) %>%
  slice_tail() %>%
  select(adjusted)

# Extract 52-week rolling high for all stocks
rolling_high_today <- close_prices %>%
  group_by(symbol) %>%
  slice_tail() %>%
  select(rolling_high)

# Extract volume rolling high for all stocks
latest_volume <- close_prices %>%
  group_by(symbol) %>%
  slice_tail() %>%
  select(volume)

# Extract 50-day average volume for all stocks
latest_avg_volume <- close_prices %>%
  group_by(symbol) %>%
  slice_tail() %>%
  select(avg_vol_50)

# Calculate volume ratio
volume_ratio <- latest_volume %>%
  left_join(latest_avg_volume) %>%
  mutate(volume_ratio = volume / avg_vol_50)

# Create final table identifying breakout candidates for export 
breakout_final <-
  volume_ratio %>%
  left_join(current_close) %>%
  left_join(rolling_high_today) %>%
  mutate(proximity = (rolling_high - adjusted) / rolling_high) %>%
  mutate(breakout = ifelse(
    proximity <= proximity_threshold & rolling_high > 0,
    'near-high',
    ''
  )) %>%
  mutate(
    breakout = ifelse(
      proximity <= soft_breakout_pct & volume_ratio > volume_threshold,
      'high-breaker',
      breakout
    )
  ) %>%
  filter(breakout == 'high-breaker' | breakout == 'near-high') %>%
  left_join(dates) %>%
  select(
    `Symbol` = symbol,
    `Date` = date,
    `Current Price` = adjusted,
    `52-week high` = rolling_high,
    `Distance to high` = proximity,
    `Volume Ratio` = volume_ratio,
    `Breakout` = breakout
  )


breakout_final %>%         #########################################
write_csv(file.choose())   ### MUST ADD .CSV TO END OF FILE NAME ###
                           #########################################





