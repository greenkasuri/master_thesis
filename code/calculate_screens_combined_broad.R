library(carteldetection)

load("../data/combined_broad.RData")

# alter the detection package to calculate screens and model separately
source("carteldetection_calculate_screens.R")


# CALCULATE SCREENS -------------------------------------------------------

screens_original <- carteldetection:::screens(processed[,-c(1, 2)], partial = TRUE, numberbids = TRUE)
screens_withMPG <- carteldetection:::screens(processed[,-c(1, 2)], MGP_quantities[,-1], MGP_quantities_accepted[,-1], partial = TRUE, numberbids = TRUE)

save(screens_original, screens_withMPG, file="../data/screens_combined_broad.RData")