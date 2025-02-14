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
non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = ymd("2016-12-31"), by = "day")

# convert dates into integers to match the case of the raw data
collusion_dates <- format(collusion_dates, "%Y%m%d")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

# collusion units
collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")

# collusive partners market, during certain collusion and after collusion
df_relevant <- function(){df[UNIT_REFERENCE_NO %in% collusive_units &
                               PURPOSE_CD=="OFF" &
                               SCOPE=="AS" &
                               (BID_OFFER_DATE_DT %in% collusion_dates | BID_OFFER_DATE_DT %in% non_collusion_dates) &
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
  
  group_by(time_stamp) %>%
  
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

# remove fully NA columns
equal_size_sample <- equal_size_sample[, .SD, .SDcols = which(colSums(is.na(equal_size_sample)) != nrow(equal_size_sample))]

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
save(equal_size_sample, file="2016_MSD_collusive_data.RData")



# CARTEL DETECTION PACKAGE ------------------------------------------------

# free up RAM
rm(df, preprocessed)

library(carteldetection)

# train
model <- carteltraintest(collusion=equal_size_sample[,1], prices=equal_size_sample[,-1], nsim=100, partial=FALSE, numberbids=TRUE)
model$results

# demeaned
model_demeaned <- carteltraintest(collusion=equal_size_sample[,1], prices=equal_size_sample[,-1], nsim=100, partial=FALSE, numberbids=TRUE, xstandardize=1)
model_demeaned$results

# standardized
model_standardized <- carteltraintest(collusion=equal_size_sample[,1], prices=equal_size_sample[,-1], nsim=100, partial=FALSE, numberbids=TRUE, xstandardize=2)
model_standardized$results



# PREDICT (if necessary) --------------------------------------------------

# predict iteravely for pairs
predicts <- list()

predict_collusion_pair <- function(pair){
  tryCatch(
    {
      cartelprediction(model, training_data[(pair*2-1):(pair*2),-1])$predictions
    }, error = function(e){
      matrix(nrow=2, ncol=1)
    }
  )
}

for (iteration in 1:(nrow(training_data)/2)){
  predicts[[iteration]] <- predict_collusion_pair(iteration)
}

# rbind all results
predicts <- do.call(rbind, predicts)



# SAVING ------------------------------------------------------------------

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\GME\\intermediate_results")

# models
save(model, file="2010_MSD_collusive_model.RData")
save(model_demeaned, file="2010_MSD_collusive_model_demeaned.RData")
save(model_standardized, file="2010_MSD_collusive_model_standardized.RData")

# save predictions
save(predicts, file="interim_results.RData")
# load("interim_results.RData")