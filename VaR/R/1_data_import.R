
# =========================================== #
# Data Import and Portfolio Setup
# =========================================== #

# Load required packages
library(tidyverse)
library(quantmod)
library(lubridate)

# suppress warnings
options(warn = -1)

# =========================================== #
# Define portfolio holdings
# =========================================== #

portfolio <- tibble(
  ticker = c(
    'AAPL', 'MSFT', 'GOOG',
    'JPM', 'BAC',
    'JNJ', 'UNH',
    'WMT', 'PG', 'XOM',
    'TSM', 'CAT'
  ),
  name = c(
    'Apple', 'Microsoft', 'Google',
    'JP Morgan', 'Bank of America',
    'Johnson & Johnson', 'UnitedHealth',
    'Walmart', 'Proctor & Gamble',
    'Taiwan Semiconductor', 'Exxon Mobil',
    'Caterpillar'
  ),
  sector = c('Technology', 'Technology', 'Technology',
             'Financials', 'Financials',
             'Healthcare', 'Healthcare',
             'Consumer', 'Consumer',
             'Technology', 'Energy', 'Industrials'
             ),
  region = c(
    rep('US', 9),
    'International',
    rep('US', 2)
  ),
  equal_weight = rep(1/12, 12)
)

# Confirm weights sum to 1
cat('Total Portfolio Weight: ', round(sum(portfolio$equal_weight), 4), "\n\n")

# Display portfolio composition
cat("Equal-Weight Portfolio Composition:\n")
cat("Each position: ", round(100/12, 2), "%\n\n", sep="")
print(portfolio %>% select(ticker, name, sector, region, equal_weight))

# =========================================== #
# Set time period and download data
# =========================================== #

# Define date range
end_date <- Sys.Date() - 1
start_date <- end_date - years(5)

cat("\n\nDownloading price data from", as.character(start_date),
    "to", as.character(end_date), "\n")

# Download adjusted close prices
price_data <- list()

for(i in 1:nrow(portfolio)){
  ticker <- portfolio$ticker[i]
  
  tryCatch({
    stock_data <- getSymbols(ticker,
                             src = 'yahoo',
                             from = start_date,
                             to = end_date,
                             auto.assign = FALSE)
    adj_close <- Ad(stock_data)
    price_data[[ticker]] <- data.frame(
      date = index(adj_close),
      price = as.numeric(adj_close),
      ticker = ticker
    )
    
    cat('Downloaded', ticker, '\n')
    
  }, error = function(e) {
    cat('Error downloading', ticker, ':', e$message, '\n')
  })
  
  Sys.sleep(0.5)
}

# Combine all price data
prices_df <- bind_rows(price_data) %>%
  arrange(ticker, date)

# Summary stats for the entire dataset
cat("\n--- Overall Data Summary ---\n")
cat("Total observations:", nrow(prices_df), "\n")
cat("Date range:", min(prices_df$date), "to", max(prices_df$date), "\n")
cat("Number of securities:", n_distinct(prices_df$ticker), "\n")

# =========================================== #
# Save the data
# =========================================== #

saveRDS(portfolio, "data/processed/portfolio_definition.rds")
saveRDS(prices_df, "data/processed/price_data.rds")
