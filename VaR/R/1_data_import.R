
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
    'AAPL', 'MFST', 'GOOG',
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
