library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(corrgram)
library(zoo)

setwd("C:/Users/jerep/Downloads/raw_data/day_ahead")

# read efficiently
df <- fread("day_ahead.csv")

head(df)

collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)
collusive_units <- c("UP_CNTRLDTVRL_1", "UP_SPARANISE_1", "UP_SPARANISE_2", "UP_NAPOLIL_4")

# lazy reference to avoid storing data multiple times
df_offer <- function(){df[PURPOSE_CD=="OFF" & BILATERAL_IN=="FALSE"]}
df_collusive <- function(){df[PURPOSE_CD=="OFF" & BILATERAL_IN=="FALSE" & UNIT_REFERENCE_NO %in% collusive_units]}

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only. Dplyr syntax necessary for dynamic variable naming
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}



# PRELIMINARY EXPLORATION/CLEANING ----------------------------------------
table(df[, INTERVAL_NO]) # strange, few bids/offers are set for interval (hour) 25...
table(df[INTERVAL_NO==25, BID_OFFER_DATE_DT]) # they occurred on October 28th and 31st, daylight savings time!
df <- df[INTERVAL_NO<25] # let's remove them for now

table(df[, STATUS_CD]) # represents whether the bid was accepted or rejected. The other codes are various exceptions
table(df[, BILATERAL_IN]) # bilateral agreements
table(df[, TYPE_CD]) # only REG


all_units <- unique(df_offer()[,c("UNIT_REFERENCE_NO", "OPERATORE")]) # all units participating in the relevant market

df[PURPOSE_CD=="OFF" & BILATERAL_IN=="FALSE" & UNIT_REFERENCE_NO %in% collusive_units & BID_OFFER_DATE_DT==20100901 & INTERVAL_NO==12]



# EXPLORATORY Q1 ----------------------------------------------------------
# How does the number of bidders evolve over time?

table(df_offer()[,ENERGY_PRICE_NO])

# number of total offers per hour
df_offer() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, .N), by=time_stamp] %>% # .N is for the number of observations
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=N), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=N), color="red", size=3)
# eligible units MUST BID in this market and the collusive partners still bid on those days. We should not notice a difference

# number of different accepted prices per hour
df_collusive() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, .N), by=time_stamp] %>% # .N is for the number of observations
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=N), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=N), color="red", size=3)
# The pattern is not clear only from this


# for bids (not always present if the TSO does not need to decrease production)
df_bid() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, .N), by=time_stamp] %>% # .N is for the number of observations
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=N), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=N), color="red", size=3)


# let's visualize the offers in the market that was relevant for the court: for the specific units that constituted a local monopoly
df_collusive() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, .N), by=time_stamp] %>% # .N is for the number of observations so number of accepted bids (offers) per day
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=N), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=N), color="red", size=3)
# hardly visible difference

# table of highlighted days
collusive_days_offers <- df_offer() %>%
  .[STATUS_CD=="ACC" & BID_OFFER_DATE_DT %in% collusion_dates, ]

collusive_days_offers_summary <- df_offer() %>%
  .[STATUS_CD=="ACC" & BID_OFFER_DATE_DT %in% collusion_dates, .N, by=.(BID_OFFER_DATE_DT, UNIT_REFERENCE_NO, ENERGY_PRICE_NO)]



# EXPLORATION Q2 ----------------------------------------------------------
# How do quantity offers evolve over time?

# average accepted offered quantity per hour
df_collusive() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, mean(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=V2), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=V2), color="red", size=3)
# does not seem higher than usual

# average accepted paid price per hour (should be the same, since this is a pay-as-bid market)
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, mean(AWARDED_PRICE_NO)), by=time_stamp] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=V2), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=V2), color="red", size=3)
# does not seem any higher either. This is in a context of falling marginal costs however.

# average accepted offered price per hour (comparing with Sundays)
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, mean(ENERGY_PRICE_NO)), by=time_stamp] %>%
  
  # filter only Sundays or highlights
  .[highlight==TRUE | wday(time_stamp)==1] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=V2), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=V2), color="red", size=3)

# average offered price per hour (on Sundays)
df_collusive() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, mean(ENERGY_PRICE_NO)), by=time_stamp] %>%
  
  # filter only Sundays or highlights
  .[highlight==TRUE | wday(time_stamp)==1] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=V2), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=V2), color="red", size=3)
# finally we see some pattern! Bid prices tend to increase and spread seems to be larger. Distribution seems to change.

# average offered quantity per hour (on Sundays)
df_collusive() %>%
  .[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, mean(QUANTITY_NO)), by=time_stamp] %>%
  
  # filter only Sundays or highlights
  .[highlight==TRUE | wday(time_stamp)==1] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=V2), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=V2), color="red", size=3)



# EXPLORATION Q3 ----------------------------------------------------------
# When did each of the collusive units win?

# let's match the table provided by the EU.
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units & BID_OFFER_DATE_DT %in% collusion_dates,] %>%
  
  .[STATUS_CD=="ACC", by=.(UNIT_REFERENCE_NO, BID_OFFER_DATE_DT)] %>%
  
  .[, dcast(.SD, BID_OFFER_DATE_DT ~ UNIT_REFERENCE_NO, fun.aggregate = function(x) length(x) > 0, value.var = "UNIT_REFERENCE_NO")]
# this is an exact match


# EXPLORATION Q4 ----------------------------------------------------------
# How many scopes can be bid on by a single competitor at the same hour?

df %>%
  # filter first to lighten the computational load
  .[PURPOSE_CD=="OFF"] %>%
  
  time_stamping() %>%
  
  # number of scopes per unit per hour
  .[, uniqueN(SCOPE), by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  .[,V1] %>%
  
  table()
# most generators bid on 2 of these markets per hour. However, this can vary from 1-5 bids, likely depending on production capacity and if their MPG bids have been accepted.

# Which scopes are offered at the same time/not?
scopes_matrix <- df %>%
  # filter first to lighten the computational load
  .[PURPOSE_CD=="OFF"] %>%
  
  time_stamping() %>%
  
  .[, SCOPE, by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  .[, dcast(.SD, time_stamp + UNIT_REFERENCE_NO ~ SCOPE, fun.aggregate = length, value.var = "SCOPE")] %>%
  
  .[, .(AS, GR1, GR2, GR3, RS)]

corrgram(scopes_matrix)
# AS is almost always matched with some GR1 but not GR2. Sometimes GR3 and RS
# GRs are usually matched with other GRs
# the higher the offers, the more likely a unit will also bid a shutting down
# unclear results

# EXPLORATION Q5 ----------------------------------------------------------
# How many relevant NA bids are there? How do these numbers compare with MGP market?

# only look in units that are active during this time i.e. that has a non-na bid at least once
participating_units <- df_offer() %>%
  .[(STATUS_CD=="ACC" | STATUS_CD=="REJ") & !(is.na(ENERGY_PRICE_NO)), UNIT_REFERENCE_NO] %>%
  unique()
# out of the 1523 units, 1296 actively participated that year

df_offer() %>%
  .[(STATUS_CD=="ACC" | STATUS_CD=="REJ") & UNIT_REFERENCE_NO %in% participating_units] %>%
  .[,sum(is.na(ENERGY_PRICE_NO))]
# 0 participating units bid NAs. Great!

# how many 0s?
df_offer() %>%
  .[(STATUS_CD=="ACC" | STATUS_CD=="REJ") & UNIT_REFERENCE_NO %in% participating_units] %>%
  .[,sum(ENERGY_PRICE_NO==0)]
# however, many bids were with 0s (4'520'816)

df_offer() %>%
  .[(STATUS_CD=="ACC" | STATUS_CD=="REJ") & UNIT_REFERENCE_NO %in% participating_units] %>%
  .[,.N]
# ... out of 8'832'725 observations. That's a lot!

# how many of the 0 bids were accepted?
df_offer() %>%
  .[STATUS_CD=="ACC" & UNIT_REFERENCE_NO %in% participating_units, sum(ENERGY_PRICE_NO==0)]
# all except 3 were accepted!! Offering 0 means offering to win by simply accepting the market price

# let's graph the accepted offers as candlesticks for the month of May
df_offer() %>%
  .[STATUS_CD=="ACC" & UNIT_REFERENCE_NO %in% participating_units]%>%
  
  time_stamping() %>%
  
  .[month(time_stamp)==5] %>%
  
  ggplot() +
  geom_boxplot(aes(x=time_stamp, y=ENERGY_PRICE_NO, group=day(time_stamp)), outlier.shape=NA) +
  coord_cartesian(ylim=c(0,25))
# accepted offers tend to be around 0. In fact, it is not significantly different from 0. We need to include them!

# let's graph the number of participating units in the Campania region
df_offer() %>%
  .[ZONE_CD=="CSUD" & (STATUS_CD=="ACC" | STATUS_CD=="REJ"), .(BID_OFFER_DATE_DT, UNIT_REFERENCE_NO, INTERVAL_NO, highlight=BID_OFFER_DATE_DT %in% collusion_dates)] %>%
  
  time_stamping() %>%
  
  .[, .(highlighted=sum(highlight)>0, no_units=length(unique(UNIT_REFERENCE_NO))), by=time_stamp] %>%
  
  ggplot() +
  geom_point(aes(x=time_stamp, y=no_units, color=highlighted))

# let's also plot quantity vs. price bids in the Campania region
df_offer() %>%
  .[ZONE_CD=="CSUD" & (STATUS_CD=="ACC" | STATUS_CD=="REJ") & ENERGY_PRICE_NO<2000, .(QUANTITY_NO, ENERGY_PRICE_NO, BID_OFFER_DATE_DT, UNIT_REFERENCE_NO, highlight=BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)] %>%
  
  ggplot() +
  geom_point(aes(x=QUANTITY_NO, y=ENERGY_PRICE_NO, color=highlight))
# this definitely reflects the withholding nature: there are no highlighted dots because the cheaters simply did not participate.

# let's plot total accepted bid quantity. It makes sense to look at this since even one unit might be important
df_offer() %>%
  .[ZONE_CD=="CSUD" & STATUS_CD=="ACC", .(BID_OFFER_DATE_DT, INTERVAL_NO, QUANTITY_NO, highlight=BID_OFFER_DATE_DT %in% collusion_dates)] %>%
  
  time_stamping() %>%
  
  .[, .(highlighted=sum(highlight)>0, quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=time_stamp, y=quantity), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=time_stamp, y=quantity), color="red", size=3)
# generally very low

# EXPLORATION Q7 ----------------------------------------------------------
# Can we identify all collusive behavior based on a rolling-window anomaly detection of accepted bids?
window <- 24*7

rolling_window <- df_offer() %>%
  .[ZONE_CD=="CSUD" & STATUS_CD=="ACC", .(BID_OFFER_DATE_DT, INTERVAL_NO, QUANTITY_NO, highlight=BID_OFFER_DATE_DT %in% collusion_dates)] %>%
  
  time_stamping() %>%
  
  .[, .(highlighted=sum(highlight)>0, quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  .[, .(highlighted, quantity, time_stamp, 
        rolling_average_quantity=rollapply(quantity, width=window, fill=NA, FUN=mean), 
        rolling_sd_quantity=rollapply(quantity, width=window, fill=NA, FUN=sd))] %>%
  
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

hist(rolling_window[highlighted==TRUE, z_score_quantity])

# z_score to include 95% of results
quantile(rolling_window[highlighted==TRUE, z_score_quantity], 0.95)
# at -.59 sd, we have 95% of collusive bidding periods

rolling_window[abs(z_score_quantity)>0.59,.N] / rolling_window[, .N]
# however, we would be including 62% of all observations, hardly a successful anomaly detection if we identify 62% of all observations as anomalies...

# let's try again but with another feature: the total number of bidders
no_bidders <- df_offer() %>%
  .[ZONE_CD=="CSUD" & (STATUS_CD=="ACC" | STATUS_CD=="REJ"), .(BID_OFFER_DATE_DT, INTERVAL_NO, UNIT_REFERENCE_NO)] %>%
  
  time_stamping() %>%
  
  .[, .(no_bidders=length(unique(UNIT_REFERENCE_NO))), by=time_stamp] %>%
  
  .[, .(no_bidders, time_stamp, 
        rolling_average_no_bidders=rollapply(no_bidders, width=window, fill=NA, FUN=mean), 
        rolling_sd_no_bidders=rollapply(no_bidders, width=window, fill=NA, FUN=sd))] %>%
  
  .[, z_score_no_bidders:=(no_bidders - rolling_average_no_bidders)/rolling_sd_no_bidders]

rolling_window <- merge(rolling_window, no_bidders, by="time_stamp", all.x=TRUE)

rolling_window %>%
  ggplot() +
  geom_point(data = . %>% .[highlighted==FALSE, ], aes(x=z_score_quantity, y=z_score_no_bidders), color="grey", size=2) +
  geom_point(data = . %>% .[highlighted==TRUE, ], aes(x=z_score_quantity, y=z_score_no_bidders), color="red", size=3)
# looks like we found a decent way to find all of our collusive observations!

setwd("C:\\Users\\jerep\\Documents\\thesis_models")
write.csv(rolling_window, file="rolling_window.csv", row.names=FALSE)
