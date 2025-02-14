library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(corrgram)
library(zoo)

setwd("C:/Users/jerep/Downloads/GME_2016/MGP")

# read efficiently
df <- fread("2016_MGP.csv")

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
df_relevant <- function(){df[ZONE_CD == "BRNN" &
                               PURPOSE_CD=="OFF" &
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



# ROLLING WINDOW ----------------------------------------------------------
window <- 24*7

rolling_window <- df_relevant() %>%
  .[STATUS_CD=="ACC", .(BID_OFFER_DATE_DT, INTERVAL_NO, QUANTITY_NO, highlight=BID_OFFER_DATE_DT %in% collusion_dates)] %>%
  
  time_stamping() %>%
  
  .[, .(highlighted=sum(highlight)>0, quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  .[, .(highlighted, quantity, time_stamp, 
        rolling_average_quantity=rollapply(quantity, width=window, fill=NA, FUN=mean, align="right"), 
        rolling_sd_quantity=rollapply(quantity, width=window, fill=NA, FUN=sd, align="right"))] %>%
  
  .[, z_score_quantity:=(quantity - rolling_average_quantity)/rolling_sd_quantity]

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=time_stamp, y=rolling_average_quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=time_stamp, y=rolling_average_quantity), color="red", size=3)

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=time_stamp, y=rolling_sd_quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=time_stamp, y=rolling_sd_quantity), color="red", size=3)

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=time_stamp, y=z_score_quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=time_stamp, y=z_score_quantity), color="red", size=3)
# these would not count as anomalies  

# let's try again but with another feature: the total number of bidders
no_bidders <- df_relevant() %>%
  .[STATUS_CD=="ACC" | STATUS_CD=="REJ", .(BID_OFFER_DATE_DT, INTERVAL_NO, UNIT_REFERENCE_NO)] %>%
  
  time_stamping() %>%
  
  .[, .(no_bidders=length(unique(UNIT_REFERENCE_NO))), by=time_stamp] %>%
  
  .[, .(no_bidders, time_stamp, 
        rolling_average_no_bidders=rollapply(no_bidders, width=window, fill=NA, FUN=mean, align="right"), 
        rolling_sd_no_bidders=rollapply(no_bidders, width=window, fill=NA, FUN=sd, align="right"))] %>%
  
  .[, z_score_no_bidders:=(no_bidders - rolling_average_no_bidders)/rolling_sd_no_bidders]

rolling_window <- merge(rolling_window, no_bidders, by="time_stamp", all.x=TRUE)

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=z_score_quantity, y=z_score_no_bidders), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=z_score_quantity, y=z_score_no_bidders), color="red", size=3)
# unfortunately, the time_window of 7 days does not capture a cartel that operates for multiple weeks.
# only the first few cartel days may be flagged but not the remaining.

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=time_stamp, y=quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=time_stamp, y=quantity), color="red", size=3)

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=time_stamp, y=no_bidders), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=time_stamp, y=no_bidders), color="red", size=3)

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
write.csv(rolling_window, file="2016_rolling_window.csv", row.names=FALSE)
