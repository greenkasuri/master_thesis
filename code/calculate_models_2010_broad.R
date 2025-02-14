library(carteldetection)

load("../data/2010_MSD_broad.RData")
load("../data/screens_2010_broad.RData")

# alter the detection package to include MGP screens (and provide a speed boost)
source("carteldetection_calculate_screens.R")

# ORIGINAL SCREENS --------------------------------------------------------

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="model_2010_broad_originalscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_2010_broad_originalscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_2010_broad_originalscreens_standardized.RData")


# MODIFIED SCREENS --------------------------------------------------------

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="model_2010_broad_MGPscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_2010_broad_MGPscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_2010_broad_MGPscreens_standardized.RData")