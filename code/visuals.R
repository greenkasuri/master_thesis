library(tidyverse)
library(data.table)
library(ggplot2)
library(lubridate)


# 2010 MSD ----------------------------------------------------------------

# read efficiently
df <- fread("C:/Users/jerep/Downloads/raw_data/ancillary/ancillary.csv")

# case information
collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)

non_collusion_dates <- seq.Date(from = ymd("2010-10-24"), to = ymd("2010-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_CNTRLDTVRL_1", "UP_SPARANISE_1", "UP_SPARANISE_2", "UP_NAPOLIL_4")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="CSUD" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
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

campania <- df_relevant()

# create a collusion column
campania = campania[, highlight:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)]

# create a time stamp column
campania <- time_stamping(campania)


# 2016 MSD ----------------------------------------------------------------

# read efficiently
df <- fread("C:/Users/jerep/Downloads/GME_2016/MSD/2016_MSD.csv")

# case information
collusion_dates <- seq.Date(from = ymd("2016-04-21"), to = ymd("2016-06-15"), by = "day")
collusion_dates <- format(collusion_dates, "%Y%m%d")

non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = ymd("2016-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # products for switching on
    SCOPE=="AS" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
]}


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

brindisi <- df_relevant()

# create a collusion column
brindisi <- brindisi[, highlight:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)]

# create a time stamp column
brindisi <- time_stamping(brindisi)

# VISUALS -----------------------------------------------------------------

# free necessary memory
rm(list = setdiff(ls(), c("campania", "brindisi", "time_stamping")))
gc()

# set global theme 
theme_set(theme_minimal())
update_geom_defaults("point", list(alpha = 0.05))

# bids and average price per hour
plot <- campania %>%

  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  stat_summary(fun = mean, geom = "line", color = "black") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Price (€/MWh)")

ggsave("../results/2010_MSD_prices.png", plot=plot, height=300, width=500, units="px", scale=5)

plot <- brindisi %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  stat_summary(fun = mean, geom = "line", color = "black") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Price (€/MWh)")

ggsave("../results/2016_MSD_prices.png", plot=plot, height=300, width=500, units="px", scale=5)

# 2010 MGP ----------------------------------------------------------------

rm(list=ls())
gc()

# read efficiently
df <- fread("C:/Users/jerep/Downloads/raw_data/day_ahead/day_ahead.csv")

# case information
collusion_dates <- c(20100509, 20100516, 20100523, 20100530, 20100602, 20100606, 20100613, 20100620, 20100627, 20100704, 20100711, 20100718, 20100725, 20100801, 20100822, 20100829, 20100905, 20100912, 20100919, 20100926, 20101003, 20101010, 20101017, 20101024)

non_collusion_dates <- seq.Date(from = ymd("2010-10-24"), to = ymd("2010-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_CNTRLDTVRL_1", "UP_SPARANISE_1", "UP_SPARANISE_2", "UP_NAPOLIL_4")

# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="CSUD" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
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

campania <- df_relevant()

# create a collusion column
campania = campania[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates]

# ... and a column for collusion units and date
campania <- campania[, highlight_units_date:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)]

# create a time stamp column
campania <- time_stamping(campania)

# load 2016 ---------------------------------------------------------------

df <- fread("C:/Users/jerep/Downloads/GME_2016/MGP/2016_MGP.csv")

# case information
collusion_dates <- seq.Date(from = ymd("2016-04-21"), to = ymd("2016-06-15"), by = "day")
collusion_dates <- format(collusion_dates, "%Y%m%d")

non_collusion_dates <- seq.Date(from = ymd("2016-06-15"), to = ymd("2016-12-31"), by = "day")
non_collusion_dates <- format(non_collusion_dates, "%Y%m%d")

collusive_units <- c("UP_SRGNPGLCNT_1", "UP_BRNDSSUDCE_1", "UP_BRNDSSUDCE_3", "UP_BRNDSSUDCE_4")


# relevant market
df_relevant <- function(){df[
  # relevant zone
  ZONE_CD=="BRNN" &
    # offers to produce (no bids to buy)
    PURPOSE_CD=="OFF" &
    # either accepted or rejected
    (STATUS_CD=="ACC" | STATUS_CD=="REJ")
]}

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

brindisi <- df_relevant()

# create a collusion column
brindisi <- brindisi[, highlight:=BID_OFFER_DATE_DT %in% collusion_dates]

# ... and a column for collusion units and date
brindisi <- brindisi[, highlight_units_date:=(BID_OFFER_DATE_DT %in% collusion_dates & UNIT_REFERENCE_NO %in% collusive_units)]

# create a time stamp column
brindisi <- time_stamping(brindisi)


# VISUALS -----------------------------------------------------------------

# free necessary memory
rm(list = setdiff(ls(), c("campania", "brindisi", "time_stamping")))
gc()

# set global theme 
theme_set(theme_minimal())
update_geom_defaults("point", list(alpha = 0.05))

# bids and average price per hour
plot <- campania %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  
  geom_point(data = . %>% .[highlight_units_date==FALSE, ], color="grey") +
  stat_summary(fun = mean, geom = "line", color = "black") +
  geom_point(data = . %>% .[highlight_units_date==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Price (€/MWh)")

ggsave("../results/2010_MGP_prices.png", plot=plot, height=300, width=500, units="px", scale=5)

plot <- brindisi %>%
  
  ggplot(aes(x=time_stamp, y=ENERGY_PRICE_NO)) +
  
  geom_point(data = . %>% .[highlight_units_date==FALSE, ], color="grey") +
  stat_summary(fun = mean, geom = "line", color = "black") +
  geom_point(data = . %>% .[highlight_units_date==TRUE, ], color="#F8776D")+
  labs(x="Hour", y="Price (€/MWh)")

ggsave("../results/2016_MGP_prices.png", plot=plot, height=300, width=500, units="px", scale=5)

# number of offers per hour
plot <- campania %>%
  .[, .(highlight, .N), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=N)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Number of offers")

ggsave("../results/2010_offers.png", plot=plot, height=300, width=500, units="px", scale=5)

plot <- brindisi %>%
  .[, .(highlight, .N), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=N)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Number of offers")

ggsave("../results/2016_offers.png", plot=plot, height=300, width=500, units="px", scale=5)


# offer quantity per hour
plot <- campania %>%
  .[, .(highlight, accepted_quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=accepted_quantity)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Total offer quantity (MWh)")

ggsave("../results/2010_quantity.png", plot=plot, height=300, width=500, units="px", scale=5)

plot <- brindisi %>%
  .[, .(highlight, accepted_quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=accepted_quantity)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Total offer quantity (MWh)")

ggsave("../results/2016_quantity.png", plot=plot, height=300, width=500, units="px", scale=5)


# accepted offers per hour
plot <- campania %>%
  .[STATUS_CD=="ACC"] %>%
  
  .[, .(highlight, .N), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=N)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Number of accepted offers")

ggsave("../results/2010_offers_accepted.png", plot=plot, height=300, width=500, units="px", scale=5)

plot <- brindisi %>%
  .[STATUS_CD=="ACC"] %>%
  
  .[, .(highlight, .N), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=N)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Number of accepted offers")

ggsave("../results/2016_offers_accepted.png", plot=plot, height=300, width=500, units="px", scale=5)

# accepted quantity per hour
plot <- campania %>%
  .[STATUS_CD=="ACC"] %>%
  
  .[, .(highlight, accepted_quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=accepted_quantity)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Total accepted offer quantity (MWh)")

ggsave("../results/2010_quantity_accepted.png", plot=plot, height=300, width=500, units="px", scale=5)

plot <- brindisi %>%
  .[STATUS_CD=="ACC"] %>%
  
  .[, .(highlight, accepted_quantity=sum(QUANTITY_NO)), by=time_stamp] %>%
  
  ggplot(aes(x=time_stamp, y=accepted_quantity)) +
  
  geom_point(data = . %>% .[highlight==FALSE, ], color="grey") +
  geom_point(data = . %>% .[highlight==TRUE, ], color="#F8776D") +
  labs(x="Hour", y="Total accepted offer quantity (MWh)")

ggsave("../results/2016_quantity_accepted.png", plot=plot, height=300, width=500, units="px", scale=5)