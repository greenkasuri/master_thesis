library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(openxlsx)

setwd("C:/Users/jerep/Downloads/raw_data/day_ahead")

# read efficiently
df <- fread("day_ahead.csv")

collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)

# full startup secondary balance market
df_relevant <- function(){df[PURPOSE_CD=="OFF" & 
                               BILATERAL_IN=="FALSE"]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only.
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}



# PRELIMINARY CLEANING ----------------------------------------------------

# remove exceptional 25th hour bids (daylight saving)
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
  
  group_by(time_stamp) %>%
  
  # select 50 random bids
  slice_sample(n=50) %>%
  
  # arrange in descending order
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
  
  select(c(collusion, everything()))



# SUBSETTING PREPROCESSING ------------------------------------------------

# exploration
training_data <- as.data.table(preprocessed)
sprintf("Number of unique bids: %i", uniqueN(training_data))
sprintf("Number of cartel cases: %i Number of non-cartel cases: %i", table(training_data[,collusion])[2], table(training_data[,collusion])[1])

# duplicates removal
training_data <- unique(training_data)
sprintf("Number of unique cartel cases: %i Number of non-cartel cases: %i", table(training_data[,collusion])[2], table(training_data[,collusion])[1])

# sample random non-cartel observations, size of the amount of cartel cases
equal_size_sample <- training_data[collusion==0][sample(.N, size=nrow(training_data[collusion==1]))]
equal_size_sample <- rbind(equal_size_sample, training_data[collusion==1])

# further sample down to improve running time
# collusive <- equal_size_sample[collusion==1][sample(.N, size=100)]
# competitive <- equal_size_sample[collusion==0][sample(.N, size=100)]
# equal_size_sample <- rbind(collusive, competitive)

# re-sample tenders with only offers of 0 or with fewer than 2 offers. Function returns indices
needs_resampling <- function(dt){
  temp <- dt[, .(
    # sum of offers
    sum_vals = rowSums(.SD, na.rm = TRUE),
    
    # Count of non-NA values except the first column
    non_na_count = rowSums(!is.na(.SD))
    
    # SDcols defines on which columns the operation should be performed
  ), .SDcols=-1]
  
  # return the index of rows with either sum of offers==0 or where there are fewer than 2 bids
  return(temp[, .I[sum_vals==0 | non_na_count<2]])
}

# while indices are not an empty list
while (!identical(needs_resampling(equal_size_sample), integer(0))) {
  
  # for every index
  for (i in needs_resampling(equal_size_sample)) {
    
    # re-sample from the same batch of collusion or non collusion
    equal_size_sample[i] <- training_data[collusion==as.integer(equal_size_sample[i, 1])][sample(.N, size=1)]
  }
}

# remove duplicates
equal_size_sample <- unique(equal_size_sample)

# remove fully NA columns
equal_size_sample <- equal_size_sample[, .SD, .SDcols = which(colSums(is.na(equal_size_sample)) != nrow(equal_size_sample))]

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\GME\\intermediate_results\\data")
save(equal_size_sample, file="2010_MGP_whole_data.RData")



# CARTEL DETECTION PACKAGE ------------------------------------------------

# free up RAM
rm(df, preprocessed)

library(carteldetection)

# train
model <- carteltraintest(collusion=equal_size_sample[,1], prices=equal_size_sample[,-1], nsim=1, partial=TRUE, numberbids=TRUE, cvfolds=4)
model$results

model_demeaned <- carteltraintest(collusion=equal_size_sample[,1], prices=equal_size_sample[,-1], nsim=1, partial=TRUE, numberbids=TRUE, cvfolds=4, xstandardize=1)
model_demeaned$results

model_standardized <- carteltraintest(collusion=equal_size_sample[,1], prices=equal_size_sample[,-1], nsim=1, partial=TRUE, numberbids=TRUE, cvfolds=4, xstandardize=2)
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

for (iteration in 1690:(nrow(training_data)/2)){
  predicts[[iteration]] <- predict_collusion_pair(iteration)
}

# rbind all results
predicts <- do.call(rbind, predicts)



# SAVING ------------------------------------------------------------------

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\GME")

# save the model and training data
save(model, file="2010_MGP_whole_model.RData")
save(model_demeaned, file="2010_MGP_whole_model_demeaned.RData")
save(model_standardized, file="2010_MGP_whole_model_standardized.RData")
# load("GME_trained_model.RData")

# save predictions
save(predicts, file="interim_results.RData")
# load("interim_results.RData")