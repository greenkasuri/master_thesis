library(carteldetection)

load("../data/combined_broad.RData")
load("../data/screens_combined_broad.RData")

# alter the detection package to calculate screens and model separately
source("carteldetection_calculate_screens.R")

# ORIGINAL SCREENS --------------------------------------------------------

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="model_combined_broad_originalscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_combined_broad_originalscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_original, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_combined_broad_originalscreens_standardized.RData")


# MODIFIED SCREENS --------------------------------------------------------

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="model_combined_broad_MGPscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_combined_broad_MGPscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], screens_withMPG, nsim=10, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_combined_broad_MGPscreens_standardized.RData")