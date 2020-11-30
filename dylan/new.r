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





