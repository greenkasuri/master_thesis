library(carteldetection)

setwd("C:\\Users\\jerep\\OneDrive\\Documents\\thesis\\master_thesis\\data")
load("2010_MSD_geographic_predictions_data.RData")

setwd("C:\\Users\\jerep\\Documents\\thesis_models")
load("2016_MSD_geographic_model.RData")

# only predict unique cases to avoid calculating twice
predicts <- unique(preprocessed)
predicts[,1] <- NA

# predict iteratively for pairs
predict_collusion_pair <- function(pair){
  # try to predict, otherwise simply add NAs
  tryCatch(
    {cartelprediction(model, predicts[(pair*2-1):(pair*2),-1])$predictions},
    error = function(e){matrix(nrow=2, ncol=1)}
  )
}

# progress bar
pb <- txtProgressBar(min = 1, max = nrow(predicts)/2, style = 3)

# main loop
for (iteration in 1:(nrow(predicts)/2)){
  prediction_pair <- predict_collusion_pair(iteration)
  predicts[iteration*2-1, 1] <- prediction_pair[1]
  predicts[iteration*2, 1] <- prediction_pair[2]
  setTxtProgressBar(pb, iteration)
}

close(pb)

save(predicts, file="MSD_geographic_2010_2016_predictions.RData")
