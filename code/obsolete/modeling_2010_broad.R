library(carteldetection)

load("../data/2010_MSD_broad.RData")
load("../data/2010_MGP_broad.RData")

# alter the detection package to include MGP screens (and provide a speed boost)
source("carteldetection_MGPscreens.R")

# ORIGINAL SCREENS --------------------------------------------------------

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="model_2010_broad_originalscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_2010_broad_originalscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_2010_broad_originalscreens_standardized.RData")


# MODIFIED SCREENS --------------------------------------------------------

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], MGP_quantities=MGP_quantities[,-1], MGP_quantities_accepted=MGP_quantities_accepted[,-1], nsim=5, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="model_2010_broad_MGPscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], MGP_quantities=MGP_quantities[,-1], MGP_quantities_accepted=MGP_quantities_accepted[,-1], nsim=5, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="model_2010_broad_MGPscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], MGP_quantities=MGP_quantities[,-1], MGP_quantities_accepted=MGP_quantities_accepted[,-1], nsim=5, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="model_2010_broad_MGPscreens_standardized.RData")