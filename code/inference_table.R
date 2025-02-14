library(dplyr)

all_markets <- c("2010_narrow", "2010_broad", "2016_narrow", "2016_broad", "combined_narrow", "combined_broad")

all_screens = c("variance", "cv", "spread", "kurt", "diff", "diffp", "rd", "rdnor", "rdalt", 
                "skew", "ks", "MGP_offers", "MGP_quantity", "MGP_accepted_offers", "MGP_accepted_quantity", "bid_quantity")

# create a list of dataframes, one for each screen
screen_tests <- lapply(all_screens, function(screen) {
  data.frame(market=all_markets,
             stat_MW=rep(NA, length(all_markets)),
             p_MW=rep(NA, length(all_markets)),
             stat_KS=rep(NA, length(all_markets)),
             p_KS=rep(NA, length(all_markets)))
  }
  )

names(screen_tests) <- all_screens


# 2010 NARROW -------------------------------------------------------------

load("../data/screens_2010_narrow.RData")
load("../data/2010_MSD_narrow.RData")

# combine datasets
xframe <- cbind(processed[,2], screens_withMPG[,1:16])

# for every column (screen)
for (screen in 1:(ncol(xframe)-1)){
  # define test groups
  group_1 = as.numeric(unlist(xframe[xframe$collusion==0, screen+1, with=FALSE]))
  group_2 = as.numeric(unlist(xframe[xframe$collusion==1, screen+1, with=FALSE]))
  
  # create statistics
  MW = wilcox.test(group_1, group_2)
  KS = ks.test(group_1, group_2)
  
  # assign statistics
  screen_tests[[screen]][1,2] = MW$statistic
  screen_tests[[screen]][1,3] = MW$p.value
  screen_tests[[screen]][1,4] = KS$statistic
  screen_tests[[screen]][1,5] = KS$p.value
}

# 2010 BROAD --------------------------------------------------------------

load("../data/screens_2010_broad.RData")
load("../data/2010_MSD_broad.RData")

# combine datasets
xframe <- cbind(processed[,2], screens_withMPG[,1:16])

# for every column (screen)
for (screen in 1:(ncol(xframe)-1)){
  # define test groups
  group_1 = as.numeric(unlist(xframe[xframe$collusion==0, screen+1, with=FALSE]))
  group_2 = as.numeric(unlist(xframe[xframe$collusion==1, screen+1, with=FALSE]))
  
  # create statistics
  MW = wilcox.test(group_1, group_2)
  KS = ks.test(group_1, group_2)
  
  # assign statistics
  screen_tests[[screen]][2,2] = MW$statistic
  screen_tests[[screen]][2,3] = MW$p.value
  screen_tests[[screen]][2,4] = KS$statistic
  screen_tests[[screen]][2,5] = KS$p.value
}


# 2016 NARROW -------------------------------------------------------------

load("../data/screens_2016_narrow.RData")
load("../data/2016_MSD_narrow.RData")

# combine datasets
xframe <- cbind(processed[,2], screens_withMPG[,1:16])

# for every column (screen)
for (screen in 1:(ncol(xframe)-1)){
  # define test groups
  group_1 = as.numeric(unlist(xframe[xframe$collusion==0, screen+1, with=FALSE]))
  group_2 = as.numeric(unlist(xframe[xframe$collusion==1, screen+1, with=FALSE]))
  
  # create statistics
  MW = wilcox.test(group_1, group_2)
  KS = ks.test(group_1, group_2)
  
  # assign statistics
  screen_tests[[screen]][3,2] = MW$statistic
  screen_tests[[screen]][3,3] = MW$p.value
  screen_tests[[screen]][3,4] = KS$statistic
  screen_tests[[screen]][3,5] = KS$p.value
}

# 2016 broad -------------------------------------------------------------

load("../data/screens_2016_broad.RData")
load("../data/2016_MSD_broad.RData")

# combine datasets
xframe <- cbind(processed[,2], screens_withMPG[,1:16])

# for every column (screen)
for (screen in 1:(ncol(xframe)-1)){
  # define test groups
  group_1 = as.numeric(unlist(xframe[xframe$collusion==0, screen+1, with=FALSE]))
  group_2 = as.numeric(unlist(xframe[xframe$collusion==1, screen+1, with=FALSE]))
  
  # create statistics
  MW = wilcox.test(group_1, group_2)
  KS = ks.test(group_1, group_2)
  
  # assign statistics
  screen_tests[[screen]][4,2] = MW$statistic
  screen_tests[[screen]][4,3] = MW$p.value
  screen_tests[[screen]][4,4] = KS$statistic
  screen_tests[[screen]][4,5] = KS$p.value
}


# COMBINED NARROW ---------------------------------------------------------

load("../data/screens_combined_narrow.RData")
load("../data/combined_narrow.RData")

# combine datasets
xframe <- cbind(processed[,2], screens_withMPG[,1:16])

# for every column (screen)
for (screen in 1:(ncol(xframe)-1)){
  # define test groups
  group_1 = as.numeric(unlist(xframe[xframe$collusion==0, screen+1, with=FALSE]))
  group_2 = as.numeric(unlist(xframe[xframe$collusion==1, screen+1, with=FALSE]))
  
  # create statistics
  MW = wilcox.test(group_1, group_2)
  KS = ks.test(group_1, group_2)
  
  # assign statistics
  screen_tests[[screen]][5,2] = MW$statistic
  screen_tests[[screen]][5,3] = MW$p.value
  screen_tests[[screen]][5,4] = KS$statistic
  screen_tests[[screen]][5,5] = KS$p.value
}


# COMBINED BROAD ----------------------------------------------------------

load("../data/screens_combined_broad.RData")
load("../data/combined_broad.RData")

# combine datasets
xframe <- cbind(processed[,2], screens_withMPG[,1:16])

# for every column (screen)
for (screen in 1:(ncol(xframe)-1)){
  # define test groups
  group_1 = as.numeric(unlist(xframe[xframe$collusion==0, screen+1, with=FALSE]))
  group_2 = as.numeric(unlist(xframe[xframe$collusion==1, screen+1, with=FALSE]))
  
  # create statistics
  MW = wilcox.test(group_1, group_2)
  KS = ks.test(group_1, group_2)
  
  # assign statistics
  screen_tests[[screen]][6,2] = MW$statistic
  screen_tests[[screen]][6,3] = MW$p.value
  screen_tests[[screen]][6,4] = KS$statistic
  screen_tests[[screen]][6,5] = KS$p.value
}


# MERGE AND SAVE ----------------------------------------------------------

screen_tests_merged <- bind_rows(screen_tests, .id = "screen")

save(screen_tests, screen_tests_merged, file="../results/tests.RData")

write.csv(screen_tests$MGP_offers, file="../results/screen_MGP_offers.csv", row.names=FALSE)
write.csv(screen_tests$MGP_quantity, file="../results/screen_MGP_quantity.csv", row.names=FALSE)
write.csv(screen_tests$MGP_accepted_offers, file="../results/screen_MGP_accepted_offers.csv", row.names=FALSE)
write.csv(screen_tests$MGP_accepted_quantity, file="../results/screen_MGP_accepted_quantity.csv", row.names=FALSE)

write.csv(screen_tests_merged, file="../results/screen_all.csv", row.names=FALSE)
