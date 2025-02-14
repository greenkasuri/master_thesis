library(dplyr)

setwd("C:\\Users\\jerep\\Documents\\thesis_models")

models <- list.files()
results <- list()
weights <- list()

for (file in models){
  load(file)
  results[[file]] <- model$results
  weights[[file]] <- model$mlweight
  }

all_results <- bind_rows(results)
all_weights <- bind_rows(weights)

mlweights <- cbind(models, all_weights)

save(mlweights, file="../results/mlweights.RData")

write.csv(mlweights, file="../results/mlweights.csv", row.names=FALSE)