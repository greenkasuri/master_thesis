library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)

# read efficiently
df <- fread("C:/Users/jerep/Downloads/GME_2016/MSD/2016_MSD.csv")

# case information
collusion_dates <- seq.Date(from = ymd("2016-04-21"), to = ymd("2016-06-15"), by = "day")
collusion_dates <- format(collusion_dates, "%Y%m%d")

non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = ymd("2016-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")


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
    
    select(c(time_stamp, ENERGY_PRICE_NO, BID_OFFER_DATE_DT)) %>%
    
    group_by(time_stamp) %>%
    
    # arrange in descending order to easily find columns of NAs only
    arrange(desc(ENERGY_PRICE_NO)) %>%
    
    # assign a number to each bid in an hour
    mutate(bid_number=row_number()) %>%
    
    ungroup() %>%
    
    pivot_wider(
      names_from=bid_number,
      values_from=ENERGY_PRICE_NO,
      names_prefix="bid"
    ) %>%
    
    mutate(collusion = as.integer(BID_OFFER_DATE_DT %in% collusion_dates)) %>%
    
    select(!BID_OFFER_DATE_DT) %>%
    
    # reorder columns, with the date
    select(c(time_stamp, collusion, everything())) %>%
    
    as.data.table()
}

# function to process from the formatted data
processing <- function (dt, equal_train_test = FALSE) {
  dt <- dt %>%
    # remove duplicates
    .[!duplicated(dt[, 2:ncol(dt)]), ] %>%
    
    # remove fully NA columns
    .[, .SD, .SDcols = which(colSums(is.na(dt)) != nrow(dt))] %>%
    
    # re-set column as integer
    .[, collusion := as.integer(collusion)]
  
  if (equal_train_test==TRUE) {
    # sample an equal amount of collusive and non-collusive points
    non_collusive <- dt[collusion==0][sample(.N, size=nrow(dt[collusion==1]))]
    dt <- rbind(non_collusive, dt[collusion==1])
  }
  dt
}


# NARROW MARKET -----------------------------------------------------------

# relevant market (lazy assignment to avoid keeping this in memory)
df_narrow <- function(){df[
  # collusive units
  UNIT_REFERENCE_NO %in% collusive_units &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ") &
    # on collusion dates or non-collusion dates
    (BID_OFFER_DATE_DT %in% collusion_dates | BID_OFFER_DATE_DT %in% non_collusion_dates)
]}

# apply formatting
preprocessed <- processing_formatting(df_narrow())

N_collusive <- preprocessed[collusion==1, .N]
N_non_collusive <- preprocessed[collusion==0, .N]
original_N <- N_collusive + N_non_collusive
original_share_collusive <- N_collusive/original_N

cat("Original number of offers: ", original_N, "\nOriginal share of collusive offers: ", original_share_collusive)

# apply processing
processed <- processing(preprocessed, equal_train_test=TRUE)

N_collusive <- processed[collusion==1, .N]
N_non_collusive <- processed[collusion==0, .N]
N <- N_collusive + N_non_collusive
share_collusive <- N_collusive/N

cat("Final number of offers: ", N, "\nFinal share of collusive offers: ", share_collusive)
cat("Average number of participants per tender: ", mean(rowSums(!is.na(processed))-2))

save(processed, file="../data/2016_MSD_narrow.RData")

# BROAD MARKET ------------------------------------------------------------

# relevant market
df_broad <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ") &
    # on collusion dates or non-collusion dates
    (BID_OFFER_DATE_DT %in% collusion_dates | BID_OFFER_DATE_DT %in% non_collusion_dates)
]}

# apply formatting
preprocessed <- processing_formatting(df_broad())

N_collusive <- preprocessed[collusion==1, .N]
N_non_collusive <- preprocessed[collusion==0, .N]
original_N <- N_collusive + N_non_collusive
original_share_collusive <- N_collusive/original_N

cat("Original number of offers: ", original_N, "\nOriginal share of collusive offers: ", original_share_collusive)

# apply processing
processed <- processing(preprocessed, equal_train_test=TRUE)

N_collusive <- processed[collusion==1, .N]
N_non_collusive <- processed[collusion==0, .N]
N <- N_collusive + N_non_collusive
share_collusive <- N_collusive/N

cat("Final number of offers: ", N, "\nFinal share of collusive offers: ", share_collusive)
cat("Average number of participants per tender: ", mean(rowSums(!is.na(processed))-2))

save(processed, file="../data/2016_MSD_broad.RData")