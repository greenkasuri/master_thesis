library(dplyr)

# load 2010
load("../data/2010_MSD_narrow.RData")
load("../data/2010_MGP_narrow.RData")

# rename variables
MSD_2010 <- processed
MGP_2010 <- MGP_quantities
MGP_accepted_2010 <- MGP_quantities_accepted

# load 2016
load("../data/2016_MSD_narrow.RData")
load("../data/2016_MGP_narrow.RData")

# rename variables
MSD_2016 <- processed
MGP_2016 <- MGP_quantities
MGP_accepted_2016 <- MGP_quantities_accepted

# combine datasets
processed <- bind_rows(MSD_2010, MSD_2016)
MGP_quantities <- bind_rows(MGP_2010, MGP_2016)
MGP_quantities_accepted <- bind_rows(MGP_accepted_2010, MGP_accepted_2016)

save(processed, MGP_quantities, MGP_quantities_accepted, file="../data/combined_narrow.RData")