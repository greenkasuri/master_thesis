library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)

setwd("C:/Users/jerep/Downloads/raw_data/ancillary")

# read efficiently
df <- fread("ancillary.csv")

# case information
collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)

non_collusion_dates <- seq.Date(from = ymd("2010-10-24"), to = ymd("2010-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="CSUD" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ") &
    # on collusion dates or non-collusion dates
    (BID_OFFER_DATE_DT %in% collusion_dates | BID_OFFER_DATE_DT %in% non_collusion_dates)
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


# DATA PROCESSING ---------------------------------------------------------

# format data in rows of bidding sessions
preprocessed <- df_relevant() %>%
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
  
  # reorder columns
  select(c(time_stamp, collusion, everything())) %>%
  
  as.data.table()

N_collusive <- preprocessed[collusion==1, .N]
N_non_collusive <- preprocessed[collusion==0, .N]
original_N <- N_collusive + N_non_collusive
original_share_collusive <- N_collusive/original_N

cat("Original number of offers: ", original_N, "\nOriginal share of collusive offers: ", original_share_collusive)

# remove duplicates
preprocessed <- preprocessed[!duplicated(preprocessed[, 2:ncol(preprocessed)]), ]

# remove fully NA columns
preprocessed <- preprocessed[, .SD, .SDcols = which(colSums(is.na(preprocessed)) != nrow(preprocessed))]

# re-set column as integer
preprocessed[, collusion := as.integer(collusion)]

# sample an equal amount of train and test data
equal_size_sample <- preprocessed[collusion==0][sample(.N, size=nrow(preprocessed[collusion==1]))]
equal_size_sample <- rbind(equal_size_sample, preprocessed[collusion==1])

# sample train and test set
train_data <- equal_size_sample[, .SD[sample(.N, size = round(.N * 0.8))], by = collusion]
train_data <- train_data %>% select(time_stamp, collusion, everything())
test_data <- fsetdiff(equal_size_sample, train_data)


N_collusive <- train_data[collusion==1, .N]
N_non_collusive <- train_data[collusion==0, .N]
N <- N_collusive + N_non_collusive
share_collusive <- N_collusive/N

cat("Train number of offers: ", N, "\nTrain share of collusive offers: ", share_collusive)

N_collusive <- test_data[collusion==1, .N]
N_non_collusive <- test_data[collusion==0, .N]
N <- N_collusive + N_non_collusive
share_collusive <- N_collusive/N

cat("Test number of offers: ", N, "\nTest share of collusive offers: ", share_collusive)
cat("Average number of participants: ", mean(rowSums(!is.na(train_data))-1))

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
save(train_data, test_data, file="2010_MSD_broad.RData")


# MODEL TRAINING ----------------------------------------------------------

# free up RAM
rm(list = setdiff(ls(), c("train_data", "test_data")))
gc()

library(carteldetection)

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# train
model <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, trainshare=0.8)
model$results

save(model, file="2010_MSD_broad_model_0.RData")

# demeaned
model <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, trainshare=0.8, xstandardize=1)
model$results

save(model, file="2010_MSD_broad_model_1.RData")

# standardized
model <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, trainshare=0.8, xstandardize=2)
model$results

save(model, file="2010_MSD_broad_model_2.RData")
