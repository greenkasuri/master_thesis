library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)


# read MGP data
df <- fread("C:/Users/jerep/Downloads/GME_2016/MGP/2016_MGP.csv")


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

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}

# function to format data in rows of bidding sessions
processing_formatting <- function (dt) {
  dt %>%
    time_stamping() %>%
    
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
    
    # reorder columns, with the date
    select(c(time_stamp, everything())) %>%
    
    as.data.table()
}

# function to process the formatted data
processing <- function (dt) {
  # remove fully NA columns
  dt[, .SD, .SDcols = which(colSums(is.na(dt)) != nrow(dt))]
}


# NARROW MARKET -----------------------------------------------------------

# load training data to get the correct dates (variable name is "processed")
load("../data/2016_MSD_narrow.RData")

# function to get all offer
df_offers <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")] %>%
    # get offers on the processed dates
    time_stamping() %>%
    
    .[time_stamp %in% processed[,time_stamp]]
}

# function to get all accepted offers
df_offers_accepted <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # accepted
    STATUS_CD=="ACC"] %>%
    # get offers on the processed dates
    time_stamping() %>%
    
    .[time_stamp %in% processed[,time_stamp]]
}

# all offers
preprocessed <- processing_formatting(df_offers())
MGP_quantities <- processing(preprocessed)

cat("Average number of participants per tender: ", mean(rowSums(!is.na(MGP_quantities))-1))
cat("Average offered quantity per tender: ", mean(rowSums(MGP_quantities[,-1], na.rm=TRUE)))


# all accepted offers
preprocessed <- processing_formatting(df_offers_accepted())
MGP_quantities_accepted <- processing(preprocessed)

cat("Average number of participants per tender: ", mean(rowSums(!is.na(MGP_quantities_accepted))-1))
cat("Average offered quantity per tender: ", mean(rowSums(MGP_quantities_accepted[,-1], na.rm=TRUE)))

save(MGP_quantities, MGP_quantities_accepted, file="../data/2016_MGP_narrow.RData")


# BROAD MARKET ------------------------------------------------------------

# load training data to get the correct dates (variable name is "processed")
load("../data/2016_MSD_broad.RData")

# function to get all offer
df_offers <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")] %>%
    # get offers on the processed dates
    time_stamping() %>%
    
    .[time_stamp %in% processed[,time_stamp]]
}

# function to get all accepted offers
df_offers_accepted <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # accepted
    STATUS_CD=="ACC"] %>%
    # get offers on the processed dates
    time_stamping() %>%
    
    .[time_stamp %in% processed[,time_stamp]]
}

# all offers
preprocessed <- processing_formatting(df_offers())
MGP_quantities <- processing(preprocessed)

cat("Average number of participants per tender: ", mean(rowSums(!is.na(MGP_quantities))-1))
cat("Average offered quantity per tender: ", mean(rowSums(MGP_quantities[,-1], na.rm=TRUE)))


# all accepted offers
preprocessed <- processing_formatting(df_offers_accepted())
MGP_quantities_accepted <- processing(preprocessed)

cat("Average number of participants per tender: ", mean(rowSums(!is.na(MGP_quantities_accepted))-1))
cat("Average offered quantity per tender: ", mean(rowSums(MGP_quantities_accepted[,-1], na.rm=TRUE)))

save(MGP_quantities, MGP_quantities_accepted, file="../data/2016_MGP_broad.RData")