library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)
library(corrgram)

setwd("C:/Users/jerep/Downloads/GME_2016/MSD")

# read efficiently
df <- fread("2016_MSD.csv")

head(df)

# lazy reference to avoid storing data multiple times
df_bid <- function(){df[PURPOSE_CD=="BID" & SCOPE=="AS" & ZONE_CD=="BRNN"]} 
df_offer <- function(){df[PURPOSE_CD=="OFF" & SCOPE=="AS" & ZONE_CD=="BRNN"]} # OUR AREA OF INTEREST

# function to create a time_stamp column. Memory-intensive when stored, use on subsets only. Dplyr syntax necessary for dynamic variable naming
time_stamping <- function(dt){
  dt %>%
    # create a common string column
    mutate(time_stamp = paste0(BID_OFFER_DATE_DT, sprintf("%02d", INTERVAL_NO))) %>%
    # change format
    mutate(time_stamp = ymd_h(time_stamp))
}

# collusion dates
start_date <- ymd("2016-04-21")
end_date <- ymd("2016-06-15")

# Create a sequence of dates from start to end
date_sequence <- seq.Date(from = start_date, to = end_date, by = "day")

# Format the dates as ymd (20160421)
collusion_dates <- format(date_sequence, "%Y%m%d")
collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")



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

# accepted bid prices
df %>%
  .[STATUS_CD=="ACC" & SCOPE=="AS" & PURPOSE_CD=="OFF"] %>%
  
  time_stamping() %>%
  
  ggplot() +
  geom_point(aes(x=time_stamp, y=ENERGY_PRICE_NO))
# max accepted bid prices are 1000, meaning that anything above that is definitely a signal that a producing unit does not wish to participate.

df %>%
  .[STATUS_CD=="ACC" & SCOPE=="AS" & PURPOSE_CD=="OFF"] %>%
  arrange(desc(ENERGY_PRICE_NO))
# max is definitely 1000

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



# Q5 ----------------------------------------------------------------------
# WHICH ENEL PRODUCTON UNIT WAS COLLUSIVE?

df_offer() %>%
  .[UNIT_REFERENCE_NO == "UP_NPWRBRNDSI_8"] %>%
  
  time_stamping() %>%
  
  .[, .N, by=time_stamp] %>% # .N is for the number of observations
  
  ggplot() +
  geom_point(aes(x=time_stamp, y=N))

df_offer() %>%
  .[UNIT_REFERENCE_NO == "UP_NPWRBRNDSI_9"] %>%
  
  time_stamping() %>%
  
  .[, .N, by=time_stamp] %>% # .N is for the number of observations
  
  ggplot() +
  geom_point(aes(x=time_stamp, y=N))
# both were active in this market and were in fact not suspected of malpractice in the final report.


# Q6 ----------------------------------------------------------------------
# HOW OFTEN DOES ONE UNIT OFFER PER DAY IN THE MSD MARKET?

# Number of different AS offers (price to turn on generator from nothing) per day of one single unit (UP_SRGNPGLCNT_1 as an example)
df_offer() %>%
  .[UNIT_REFERENCE_NO == "UP_SRGNPGLCNT_1"] %>%
  
  .[, .N, by=.(BID_OFFER_DATE_DT, ENERGY_PRICE_NO)] %>% # .N is for the number of observations
  
  mutate(date_stamp = ymd(BID_OFFER_DATE_DT)) %>%
  
  ggplot() +
  geom_point(aes(x=date_stamp, y=N))
# a unit might bid only one single price per day. It may bid a different price for every hour AND it may even bid more than one price per hour. Why?

# day of most offers
df_offer() %>%
  .[UNIT_REFERENCE_NO == "UP_SRGNPGLCNT_1"] %>%
  
  .[, .N, by=.(BID_OFFER_DATE_DT, ENERGY_PRICE_NO)] %>%
  
  .[N == max(N), BID_OFFER_DATE_DT]
# on 20160903

# offers
offers_on_20160903 <- df_offer() %>%
  .[UNIT_REFERENCE_NO == "UP_SRGNPGLCNT_1" & BID_OFFER_DATE_DT == "20160903"]


# Q7 ----------------------------------------------------------------------
# DO UNITS BID MULTIPLE TIMES PER DAY?

df %>%
  
  .[PURPOSE_CD=="OFF" & SCOPE=="AS"] %>%
  
  time_stamping() %>%
  
  .[,.(.N), by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  pull(N) %>%
  
  max()
# yes, up to 7 times.

df %>%
  
  .[PURPOSE_CD=="OFF" & SCOPE=="AS" & (STATUS_CD=="ACC" | STATUS_CD=="REJ")] %>%
  
  time_stamping() %>%
  
  .[,.(.N), by=.(time_stamp, UNIT_REFERENCE_NO)] %>%
  
  pull(N) %>%
  
  max()
# solved as the 2016

# max price of an accepted bid
df %>%
  .[PURPOSE_CD=="OFF" & SCOPE=="AS" & STATUS_CD=="ACC"] %>%
  
  time_stamping() %>%
  
  ggplot() +
  geom_point(aes(x=time_stamp, y=ENERGY_PRICE_NO))
# again 1000

df %>%
  .[PURPOSE_CD=="OFF" & SCOPE=="AS" & STATUS_CD=="ACC"] %>%
  pull(ENERGY_PRICE_NO) %>%
  max()
