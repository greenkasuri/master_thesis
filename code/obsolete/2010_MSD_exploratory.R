library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(corrgram)

setwd("C:/Users/jerep/Downloads/raw_data/ancillary")

# read efficiently
df <- fread("ancillary.csv")

head(df)

# lazy reference to avoid storing data multiple times
df_bid <- function(){df[PURPOSE_CD=="BID" & SCOPE=="AS" & ZONE_CD=="CSUD"]} 
df_offer <- function(){df[PURPOSE_CD=="OFF" & SCOPE=="AS" & ZONE_CD=="CSUD"]} # OUR AREA OF INTEREST

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only. Dplyr syntax necessary for dynamic variable naming
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}

collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)
collusive_units <- c("UP_CNTRLDTVRL_1", "UP_SPARANISE_1", "UP_SPARANISE_2", "UP_NAPOLIL_4")



# PRELIMINARY EXPLORATION/CLEANING ----------------------------------------
table(df[, INTERVAL_NO]) # strange, few bids/offers are set for interval (hour) 25...
table(df[INTERVAL_NO==25, BID_OFFER_DATE_DT]) # they occurred on October 28th and 31st, daylight savings time!
df <- df[INTERVAL_NO<25] # let's remove them for now

table(df[, STATUS_CD]) # represents whether the bid was accepted or rejected. The other codes are various exceptions
table(df[, SCOPE]) # the scope is specific to the ancillary market and denotes what the unit would be willing to get from different production starting points (i.e. if the unit was off or already on when balancing services were required)

all_units <- unique(df_offer()[,c("UNIT_REFERENCE_NO", "OPERATORE")]) # all units participating in the relevant market



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

# number of different accepted prices per hour (pay-as-bid)
df_offer() %>%
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
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
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
# How does price evolve over time?

# average accepted offered price per hour
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[STATUS_CD=="ACC", .(highlight, mean(ENERGY_PRICE_NO)), by=time_stamp] %>%
  
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

# average offered price per hour
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  .[, .(highlight, mean(ENERGY_PRICE_NO)), by=time_stamp] %>%
  
  # filter only Sundays or highlights
  .[highlight==TRUE | wday(time_stamp)==1] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=V2), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=V2), color="red", size=3)
# finally we see some pattern! Bid prices tend to increase and spread seens to be larger. Distribution seems to change.


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

# quantities vs prices
df_offer() %>%
  time_stamping %>%
  ggplot() +
  geom_point(aes(x=time_stamp, y=ENERGY_PRICE_NO))

df_offer() %>%
  time_stamping %>%
  ggplot() +
  geom_point(aes(x=time_stamp, y=QUANTITY_NO))

# are quantities an integral part of the bids when offering to start up the plant?
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units] %>%
  time_stamping %>%
  ggplot() +
  geom_line(aes(x=time_stamp, y=QUANTITY_NO, color=UNIT_REFERENCE_NO))

df_offer() %>%
  .[UNIT_REFERENCE_NO=="UP_CNTRLDTVRL_1"] %>%
  time_stamping %>%
  ggplot() +
  geom_line(aes(x=time_stamp, y=QUANTITY_NO))

# IMPORTANT: bidding price of the collusive units
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units] %>%
  time_stamping %>%
  ggplot() +
  geom_line(aes(x=time_stamp, y=ENERGY_PRICE_NO, color=UNIT_REFERENCE_NO))

df[PURPOSE_CD=="BID" & SCOPE=="AS" & UNIT_REFERENCE_NO %in% collusive_units] %>%
   time_stamping %>%
   ggplot() +
   geom_line(aes(x=time_stamp, y=ENERGY_PRICE_NO, color=UNIT_REFERENCE_NO))

accepted_collusive_bids <- df %>%
  .[PURPOSE_CD=="OFF" & SCOPE=="AS" & UNIT_REFERENCE_NO %in% collusive_units & STATUS_CD=="ACC" & ENERGY_PRICE_NO==0]

accepted_0_bids <- df %>%
  .[PURPOSE_CD=="OFF" & SCOPE=="AS" & STATUS_CD=="ACC" & ENERGY_PRICE_NO==0]
# conclusion: quantities are rather arbitrary and are often adjusted by Terna itself to simply match the minimum requirement.
# However, price = 0 is never accepted

# what was the offered price of the accepted bids?
df %>%
  .[PURPOSE_CD=="OFF" & SCOPE=="AS" & STATUS_CD=="ACC"] %>%
  time_stamping() %>%
  ggplot() +
  geom_point(aes(x=time_stamp, y=ENERGY_PRICE_NO))
# never 0 that's for sure

# for collusive units
df_offer() %>%
  .[UNIT_REFERENCE_NO %in% collusive_units, highlight:=BID_OFFER_DATE_DT %in% collusion_dates] %>%
  
  time_stamping() %>%
  
  # filter only Sundays or highlights
  .[highlight==TRUE | wday(time_stamp)==1] %>%
  
  ggplot() +
  geom_point(data = . %>% .[highlight==FALSE, ], aes(x=time_stamp, y=ENERGY_PRICE_NO), color="grey", size=2) +
  geom_point(data = . %>% .[highlight==TRUE, ], aes(x=time_stamp, y=ENERGY_PRICE_NO), color="red", size=3)
# never 0 and actually there is no clear misbehavior here either. Bidders will bid high to avoid being chosen.

# Q5 - CAN A UNIT BID MULTIPLE TIMES IN ONE HOUR? -------------------------
df_relevant() %>%
  
  time_stamping() %>%
  
  .[,.(.N), by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  pull(N) %>%
  
  max()
# yes, up to 6 times per hour. What do they bet?

multiple_bids_units <- df_relevant() %>%
  
  time_stamping() %>%
  
  .[,.(.N), by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  .[N>5] %>%
  
  pull(UNIT_REFERENCE_NO) %>%
  
  unique()

test <- df_relevant() %>%
  
  .[UNIT_REFERENCE_NO %in% multiple_bids_units & ENERGY_PRICE_NO<1000] %>%
  
  time_stamping()
# mostly the same but with a different status (e.g. replaced or revoked bids due to inconsistency)

# let's try to take only accepted or rejected offers
df_relevant() %>%
  
  .[STATUS_CD=="ACC" | STATUS_CD=="REJ"] %>%
  
  time_stamping() %>%
  
  .[,.(.N), by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  pull(N) %>%
  
  max()
# perfection


# EXPLORATION Q6 ----------------------------------------------------------
# How many relevant NA bids are there? How do these numbers compare with MGP market?

# only look in units that are able to provide balancing services i.e. that bid more than 0 at least once
participating_units <- df_offer() %>%
  .[(STATUS_CD=="ACC" | STATUS_CD=="REJ") & !(is.na(ENERGY_PRICE_NO)) & ENERGY_PRICE_NO>0, UNIT_REFERENCE_NO] %>%
  unique()
# not many units actually participate in this market

df_offer() %>%
  .[(STATUS_CD=="ACC" | STATUS_CD=="REJ") & UNIT_REFERENCE_NO %in% participating_units] %>%
  .[,sum(is.na(ENERGY_PRICE_NO))]
# 0