
library(readxl)
library(tidyverse)

cb_raw <- read_xlsx('C:\\Users\\derek.peterson\\Downloads\\Calcbench Data Query Export.xlsx')

head(cb_raw)
names(cb_raw) <- cb_raw[4, ]
cb_raw <- cb_raw[-c(1:4), ]

View(cb_raw)

cb <- cb_raw %>%
  mutate(period_end = as.Date(as.numeric(period_end),
                              origin = '1899-12-30'),
         publish_date = as.Date(as.numeric(filing_date),
                                origin = '1899-12-30'))


cb_small <- cb %>%
  select(
    ticker,
    publish_date,
    period_end,
    EPSDiluted,
    StockholdersEquity,
    AvgDilutedSharesOutstanding,
    Revenue,
    CommonStockDividendsPerShare
  ) %>%
  filter(publish_date >= '2017-01-01')

write_csv(cb_small, "C:\\Derek\\R\\Finance\\Alpha\\data\\calcbench.csv")
