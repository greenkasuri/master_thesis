library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)

# load training data to acquire the same dates from MGP
setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
load("2010_MSD_broad.RData")

# load MGP data
setwd("C:/Users/jerep/Downloads/raw_data/day_ahead")

# read efficiently
df <- fread("day_ahead.csv")

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="CSUD" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ") 
  ] %>%
    time_stamping() %>%
    .[time_stamp %in% train_data[,time_stamp]]
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


# DATA PROCESSING ---------------------------------------------------------

# format data in rows of bidding sessions (without collusion indicator)
preprocessed <- df_relevant() %>%
  select(c(time_stamp, QUANTITY_NO)) %>%
  
  group_by(time_stamp) %>%
  
  # arrange in descending order to easily find columns of NAs only
  arrange(desc(QUANTITY_NO)) %>%
  
  # assign a number to each bid in an hour
  mutate(bid_number=row_number()) %>%
  
  ungroup() %>%
  
  pivot_wider(
    names_from=bid_number,
    values_from=QUANTITY_NO,
    names_prefix="bid"
  ) %>%
  
  # reorder columns
  select(c(time_stamp, everything())) %>%
  
  as.data.table()

# remove fully NA columns
preprocessed <- preprocessed[, .SD, .SDcols = which(colSums(is.na(preprocessed)) != nrow(preprocessed))]

# rename
MGP_quantities <- preprocessed
rm(preprocessed)

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
save(MGP_quantities, train_data, file="test.RData")


# TRAINING ----------------------------------------------------------------

# free up RAM
rm(list = setdiff(ls(), c("train_data", "test_data")))
gc()

library(carteldetection)

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# train
model <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=FALSE, numberbids=TRUE, trainshare=0.8)
model$results
