library(tidyverse)
library(stringr)
library(gtrendsR)
library(Quandl)
library(quantmod)
library(RcppRoll)
library(lubridate)
library(tidyquant)
library(tidymodels)
library(tsfeatures)
library(slider)
library(timetk)
library(data.table)


Quandl.api_key("5ydoG6gTCKjgzDpJp_1s")

# Creating a function to clean data retrieved using Quandl
quandl_tidy <- function(code, name){
  # Quandl retrieves data based on what is asked for
  # code is the argument in Quandl to pull a specific column of data
  df <- Quandl(code) %>%
    mutate(code=code, name=name) %>%
    rename(date=Date, value=Value) %>%
    arrange(date) %>%
    as_tibble()
  return(df)
}

# getting bitcoin price and other trading data
# open, high, low, close, cvolume
bitcoin_price <- Quandl("BCHARTS/BITSTAMPUSD") %>%
  arrange(Date) %>%
  as_tibble()


# Data about bitcoin activity, trasnaction fees and mining etc
# each item is in the list contains the code for Quandl and the accompanying column name to name it
code_list <- list(c("BCHAIN/TOTBC", "Total Bitcoins"), 
                  c("BCHAIN/MKTCP", "Bitcoin Market Capitalization"), 
                  c("BCHAIN/NADDU", "Bitcoin Number of Unique Addresses Used"), 
                  c("BCHAIN/ETRAV", "Bitcoin Estimated Transaction Volume BTC"), 
                  c("BCHAIN/ETRVU", "Bitcoin Estimated Transaction Volume USD"), 
                  c("BCHAIN/TRVOU", "Bitcoin USD Exchange Trade Volume"), 
                  c("BCHAIN/NTRAN", "Bitcoin Number of Transactions"), 
                  c("BCHAIN/NTRAT", "Bitcoin Total Number of Transactions"), 
                  c("BCHAIN/NTREP", "Bitcoin Number of Transactions Excluding Popular Addresses"), 
                  c("BCHAIN/NTRBL", "Bitcoin Number of Tansaction per Block"), 
                  c("BCHAIN/ATRCT", "Bitcoin Median Transaction Confirmation Time"), 
                  c("BCHAIN/TRFEE", "Bitcoin Total Transaction Fees"), 
                  c("BCHAIN/TRFUS", "Bitcoin Total Transaction Fees USD"), 
                  c("BCHAIN/CPTRA", "Bitcoin Cost Per Transaction"), 
                  c("BCHAIN/CPTRV", "Bitcoin Cost % of Transaction Volume"), 
                  c("BCHAIN/BLCHS", "Bitcoin api.blockchain Size"), 
                  c("BCHAIN/AVBLS", "Bitcoin Average Block Size"), 
                  c("BCHAIN/TOUTV", "Bitcoin Total Output Volume"), 
                  c("BCHAIN/HRATE", "Bitcoin Hash Rate"), 
                  c("BCHAIN/MIREV", "Bitcoin Miners Revenue"), 
                  c("BCHAIN/BCDDE", "Bitcoin Days Destroyed"), 
                  c("BCHAIN/BCDDW", "Bitcoin Days Destroyed Minimum Age 1 Week"), 
                  c("BCHAIN/BCDDM", "Bitcoin Days Destroyed Minimum Age 1 Month"), 
                  c("BCHAIN/BCDDY", "Bitcoin Days Destroyed Minimum Age 1 Year") ,
                  c("BCHAIN/BCDDC", "Bitcoin Days Destroyed Cumulative"))


# Creating an empty tibble
bitcoin_data <- tibble()



# seq_along generates a regular sequence of numbers
# just to help to iterate by numbers. similar to emunerate in python. this helps you to make it an index
# seq_along can replace i in 1:length(code_list)
for (i in seq_along(code_list)) { 
  
  print(str_c("Downloading data for ", code_list[[i]][1], "."))
  
  
  bitcoin_data <- bind_rows(bitcoin_data, 
                            quandl_tidy(code_list[[i]][1],#first item in the list is the code of the data 'column' to pull from Quandl
                                        # this is the second item in the list, just to name the column nicely
                                        code_list[[i]][2]))
}

print("Done downloading data")


bitcoin_data <- bitcoin_data %>%
  select(-name) %>%
  spread(code, value)

# make.names is to clean the names
colnames(bitcoin_data) <- make.names(colnames(bitcoin_data))



#--------------------------------------------------------------------------
# Scraping google data

# Making a column of dates
download_all <- FALSE

# if (download_all == TRUE){
#   # 10 years worth of dates, by month
#   dates <- tibble(dates = ymd("2011-01-01") + days(0:3650)) %>%
#     filter(dates <= ymd("2020-11-30"))
# } else{
#   dates <- tibble(dates=ymd("2018-01-01") + days(0:3650)) %>%
#     filter(dates <= ymd("2020-11-30"))
# }


# Create a function to pull google trends data
# limitation is the query only works by week if you do over a span

google_trends <- function(query, begin_date, end_date) {
  
  df <- gtrends(keyword = 'bitcoin',
                time = str_c(begin_date, ' ', end_date))[['interest_over_time']] %>%
    select(date, hits) %>%
    mutate(date = as.Date(date)) %>%
    as_tibble()
  
  return(df)
}

for (i in 1:nrow(dates)) {
  
  month <- dates[["dates"]][i]
  begin_date <- as.Date(month)
  end_date <- as.Date(month) + months(1) - days(1)
  end_date <- as.Date(ifelse(end_date >= Sys.Date(), Sys.Date(), end_date))
  
  df <- google_trends("bitcoin", begin_date, end_date)
  
  write_csv(df, file.path('pub_trends', str_c("google-trends-daily-", begin_date, "-", end_date, ".csv")))
  
}

monthly <- google_trends('bitcoin', '2018-01-01', '2020-11-30') %>%
  rename(hits_monthly = hits)

bitcoin_google <- list.files('pub_trends') %>%
  map_df(~ read_csv(file.path('pub_trends', str_c(.)), col_types = c('Di'))) %>%
  rename(hits_daily = hits) %>%
  left_join(monthly) %>%
  fill(hits_monthly) %>%
  mutate(hits_monthly = as.numeric(hits_monthly),
         hits_daily = hits_daily * hits_monthly / 100)