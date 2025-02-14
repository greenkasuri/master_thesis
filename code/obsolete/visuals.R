library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)


# 2010 MSD ----------------------------------------------------------------

setwd("C:/Users/jerep/Downloads/raw_data/ancillary")

# read efficiently
df <- fread("ancillary.csv")

# case information
collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)

non_collusion_dates <- seq.Date(from = ymd("2010-10-24"), to = ymd("2010-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_CNTRLDTVRL_1", "UP_SPARANISE_1", "UP_SPARANISE_2", "UP_NAPOLIL_4")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="CSUD" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}


# DATA PREPROCESSING ------------------------------------------------------

# remove exceptional 25th hour bids
extra_hour_cleaning <- function(dt){
  # print number of bids
  cat(dt[INTERVAL_NO>=25, .N], "bids for hour 25 removed on dates: \n")
  # print dates
  for (date in unique(dt[INTERVAL_NO>=25, BID_OFFER_DATE_DT])){
    cat(format(ymd(date), "%Y-%m-%d"), "\n")
  }
  # remove bids
  dt[INTERVAL_NO<25]
}

df <- extra_hour_cleaning(df)

# change 0s to NAs in the MSD market due to maintenance
maintenance <- function(dt){
  mutate(dt, ENERGY_PRICE_NO = replace(ENERGY_PRICE_NO, ENERGY_PRICE_NO==0, NA))
}

df <- maintenance(df)


# VISUALS -----------------------------------------------------------------

# bids and average price per hour
df_relevant() %>%
  .[, highlight:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)] %>%
  
  time_stamping() %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey", size=2) +
  stat_summary(fun = mean, geom = "line", color = "black", linewidth = 1) +
  geom_point(data = . %>% .[highlight==TRUE, ], color="red", size=3)


# 2016 MSD ----------------------------------------------------------------

rm(list=ls())
gc()

setwd("C:/Users/jerep/Downloads/GME_2016/MSD")

# read efficiently
df <- fread("2016_MSD.csv")

# case information
collusion_dates <- seq.Date(from = ymd("2016-04-21"), to = ymd("2016-06-15"), by = "day")
collusion_dates <- format(collusion_dates, "%Y%m%d")

non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = ymd("2016-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}


# DATA PREPROCESSING ------------------------------------------------------

# remove exceptional 25th hour bids
extra_hour_cleaning <- function(dt){
  # print number of bids
  cat(dt[INTERVAL_NO>=25, .N], "bids for hour 25 removed on dates: \n")
  # print dates
  for (date in unique(dt[INTERVAL_NO>=25, BID_OFFER_DATE_DT])){
    cat(format(ymd(date), "%Y-%m-%d"), "\n")
  }
  # remove bids
  dt[INTERVAL_NO<25]
}

df <- extra_hour_cleaning(df)

# change 0s to NAs in the MSD market due to maintenance
maintenance <- function(dt){
  mutate(dt, ENERGY_PRICE_NO = replace(ENERGY_PRICE_NO, ENERGY_PRICE_NO==0, NA))
}

df <- maintenance(df)


# VISUALS -----------------------------------------------------------------

# bids and average price per hour
df_relevant() %>%
  .[, highlight:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)] %>%
  
  time_stamping() %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey", size=2) +
  stat_summary(fun = mean, geom = "line", color = "black", linewidth = 1) +
  geom_point(data = . %>% .[highlight==TRUE, ], color="red", size=3)


# 2010 MGP ----------------------------------------------------------------

rm(list=ls())
gc()

setwd("C:/Users/jerep/Downloads/raw_data/day_ahead")

# read efficiently
df <- fread("day_ahead.csv")

# case information
collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)

non_collusion_dates <- seq.Date(from = ymd("2010-10-24"), to = ymd("2010-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_CNTRLDTVRL_1", "UP_SPARANISE_1", "UP_SPARANISE_2", "UP_NAPOLIL_4")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="CSUD" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}


# DATA PREPROCESSING ------------------------------------------------------

# remove exceptional 25th hour bids
extra_hour_cleaning <- function(dt){
  # print number of bids
  cat(dt[INTERVAL_NO>=25, .N], "bids for hour 25 removed on dates: \n")
  # print dates
  for (date in unique(dt[INTERVAL_NO>=25, BID_OFFER_DATE_DT])){
    cat(format(ymd(date), "%Y-%m-%d"), "\n")
  }
  # remove bids
  dt[INTERVAL_NO<25]
}

df <- extra_hour_cleaning(df)


# VISUALS -----------------------------------------------------------------

# number of offers per hour
df_relevant() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, .N), by=time_stamp] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=N), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=N), color="red", size=3)

# total accepted quantity per hour
df_relevant() %>%
  .[STATUS_CD=="ACC"] %>%
  
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, accepted_quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=accepted_quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=accepted_quantity), color="red", size=3)

# total bid quantity per hour
df_relevant() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, total_quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=total_quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=total_quantity), color="red", size=3)

# offer price and average per hour
df_relevant() %>%
  .[, highlight:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)] %>%
  
  time_stamping() %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey", size=2) +
  stat_summary(fun = mean, geom = "line", color = "black", linewidth = 1) +
  geom_point(data = . %>% .[highlight==TRUE, ], color="red", size=3)


# 2016 MGP ----------------------------------------------------------------

rm(list=ls())
gc()

# read efficiently
df <- fread("C:/Users/jerep/Downloads/GME_2016/MGP/2016_MGP.csv")

# case information
collusion_dates <- seq.Date(from = ymd("2016-04-21"), to = ymd("2016-06-15"), by = "day")
collusion_dates <- format(collusion_dates, "%Y%m%d")

non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = ymd("2016-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")


# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}


# DATA PREPROCESSING ------------------------------------------------------

# remove exceptional 25th hour bids
extra_hour_cleaning <- function(dt){
  # print number of bids
  cat(dt[INTERVAL_NO>=25, .N], "bids for hour 25 removed on dates: \n")
  # print dates
  for (date in unique(dt[INTERVAL_NO>=25, BID_OFFER_DATE_DT])){
    cat(format(ymd(date), "%Y-%m-%d"), "\n")
  }
  # remove bids
  dt[INTERVAL_NO<25]
}

df <- extra_hour_cleaning(df)


# VISUALS -----------------------------------------------------------------

# offer price and average per hour
df_relevant() %>%
  .[, highlight:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)] %>%
  
  time_stamping() %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey", size=2) +
  stat_summary(fun = mean, geom = "line", color = "black", linewidth = 1) +
  geom_point(data = . %>% .[highlight==TRUE, ], color="red", size=3)