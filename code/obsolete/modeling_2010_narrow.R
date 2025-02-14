library(carteldetection)

load("../data/2010_MSD_narrow.RData")
load("../data/2010_MGP_narrow.RData")

# alter the detection package to include MGP screens (and provide a speed boost)
source("carteldetection_MGPscreens.R")

# ORIGINAL SCREENS --------------------------------------------------------

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], nsim=10, partial=FALSE, numberbids=TRUE, cvfolds=5)
model$results

save(model, file="model_2010_narrow_originalscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], nsim=10, partial=FALSE, numberbids=TRUE, cvfolds=5, xstandardize=1)
model$results

save(model, file="model_2010_narrow_originalscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], nsim=10, partial=FALSE, numberbids=TRUE, cvfolds=5, xstandardize=2)
model$results

save(model, file="model_2010_narrow_originalscreens_standardized.RData")


# MODIFIED SCREENS --------------------------------------------------------

# normal
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], MGP_quantities=MGP_quantities[,-1], MGP_quantities_accepted=MGP_quantities_accepted[,-1], nsim=10, partial=FALSE, numberbids=TRUE, cvfolds=5)
model$results

save(model, file="model_2010_narrow_MGPscreens.RData")

# demeaned
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], MGP_quantities=MGP_quantities[,-1], MGP_quantities_accepted=MGP_quantities_accepted[,-1], nsim=10, partial=FALSE, numberbids=TRUE, cvfolds=5, xstandardize=1)
model$results

save(model, file="model_2010_narrow_MGPscreens_demeaned.RData")

# standardized
model <- carteldetection::carteltraintest(collusion=processed[,2], prices=processed[,-c(1, 2)], MGP_quantities=MGP_quantities[,-1], MGP_quantities_accepted=MGP_quantities_accepted[,-1], nsim=10, partial=FALSE, numberbids=TRUE, cvfolds=5, xstandardize=2)
model$results

save(model, file="model_2010_narrow_MGPscreens_standardized.RData")