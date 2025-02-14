setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")

load("2010_MSD_broad.RData")

library(carteldetection)

setwd("C:\\Users\\jerep\\Documents\\thesis_models")


print("beginning!")

# train
model <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE)
model$results

save(model, file="2010_MSD_broad_model_0.RData")

# demeaned
model <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, xstandardize=1)
model$results

save(model, file="2010_MSD_broad_model_1.RData")

# standardized
model_standardized <- carteltraintest(collusion=train_data[,2], prices=train_data[,-c(1, 2)], nsim=5, partial=TRUE, numberbids=TRUE, xstandardize=2)
model$results

save(model, file="2010_MSD_broad_model_2.RData")