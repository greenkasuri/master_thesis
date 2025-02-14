library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(openxlsx)

setwd("C:/Users/jerep/Downloads/GME_2016/MSD")

# read efficiently
df <- fread("2016_MSD.csv")

# collusion dates
start_date <- ymd("2016-04-21")
end_date <- ymd("2016-06-15")
collusion_dates <- seq.Date(from = start_date, to = end_date, by = "day")
non_collusion_end_date <- end_date + length(collusion_dates)
non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = non_collusion_end_date, by = "day")

# convert dates into integers to match the case of the raw data
collusion_dates <- format(collusion_dates, "%Y%m%d")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

# collusion units
collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")

# collusive partners market, during certain collusion and after collusion
df_relevant <- function(){df[ZONE_CD ==  "BRNN" &
                               PURPOSE_CD=="OFF" &
                               SCOPE=="AS" &
                               (STATUS_CD=="ACC" | STATUS_CD=="REJ")]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}

# PRELIMINARY CLEANING ----------------------------------------------------

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



# PREPROCESSING -----------------------------------------------------------

# format data in rows of bidding sessions
preprocessed <- df_relevant() %>%
  time_stamping() %>%
  
  select(c(time_stamp, ENERGY_PRICE_NO, BID_OFFER_DATE_DT)) %>%
  
  # on the MSD AS market, a bid of 0 €/MWh means that the unit exceptionally cannot operate
  mutate(ENERGY_PRICE_NO = replace(ENERGY_PRICE_NO, ENERGY_PRICE_NO==0, NA)) %>%
  
  # on the MSD AS market, bids above 1000 €/MWh are never accepted (the legal limit is 3000)
  .[ENERGY_PRICE_NO <= 1000] %>%
  
  group_by(time_stamp) %>%
  
  # sort in descending order to have all the NAs on one side
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
  
  select(!c(time_stamp, BID_OFFER_DATE_DT)) %>%
  
  select(c(collusion, everything())) %>%
  
  as.data.table()



# SUBSETTING PREPROCESSING ------------------------------------------------

# exploration
training_data <- as.data.table(preprocessed)
sprintf("Number of cartel cases: %i Number of non-cartel cases: %i", table(training_data[,collusion])[2], table(training_data[,collusion])[1])

# duplicates removal
training_data <- distinct(training_data)
sprintf("Number of unique cartel cases: %i Number of non-cartel cases: %i", table(training_data[,collusion])[2], table(training_data[,collusion])[1])

# sample random non-cartel observations, size of the amount of cartel cases
equal_size_sample <- training_data[collusion==0][sample(.N, size=nrow(training_data[collusion==1]))]
equal_size_sample <- rbind(equal_size_sample, training_data[collusion==1])

# remove duplicates
equal_size_sample <- unique(equal_size_sample)

# remove fully NA columns
equal_size_sample <- equal_size_sample[, .SD, .SDcols = which(colSums(is.na(equal_size_sample)) != nrow(equal_size_sample))]

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
save(equal_size_sample, file="2016_MSD_geographic_data.RData")
