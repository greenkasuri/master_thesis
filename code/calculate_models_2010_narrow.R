library(carteldetection)

load("../data/2010_MSD_narrow.RData")
load("../data/screens_2010_narrow.RData")

# alter the detection package to include MGP screens (and provide a speed boost)
source("carteldetection_calculate_screens.R")

# ORIGINAL SCREENS --------------------------------------------------------

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=FALSE, numberbids=TRUE)
model$results

save(model, file="model_2010_narrow_originalscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=FALSE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_2010_narrow_originalscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=FALSE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_2010_narrow_originalscreens_standardized.RData")


# MODIFIED SCREENS --------------------------------------------------------

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=FALSE, numberbids=TRUE)
model$results

save(model, file="model_2010_narrow_MGPscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=FALSE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_2010_narrow_MGPscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=FALSE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_2010_narrow_MGPscreens_standardized.RData")