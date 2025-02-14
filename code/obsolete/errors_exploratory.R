library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)

# count NAs
print("Total NAs:")
(total_nas <- sum(is.na(equal_size_sample)))

# max NAs per row
print("Max NAs per row as a percentage of total bids per day:")
nas_per_row <- apply(equal_size_sample, 1, function(x) sum(is.na(x)))
max(nas_per_row / (ncol(equal_size_sample)-1))

# count 0s and NAs
print("Total 0s and NAs:")
total_nas + sum(equal_size_sample==0)

# max 0s and NAs per row
print("Max NAs and 0s per row as a percentage of total bids per day:")
max((nas_per_row + apply(equal_size_sample, 1, function(x) sum(x==0))) / (ncol(equal_size_sample)-1))

# max duplicate values per row
print("Max duplicates per row as a percentage of non-NA bids for that row:")
duplicates_percentage <- function(x) (length(na.omit(x)) - length(unique(na.omit(x)))) / length(na.omit(x))
max(apply(equal_size_sample, 1, duplicates_percentage))

# minimum variance per row
print("Minimum variance per row")
min(apply(equal_size_sample, 1, function(x) var(x, na.rm=TRUE)))

# minimum difference in highest and lowest bid per row
print("Minimum difference in highest and lowest bid:")
min(apply(equal_size_sample, 1, function(x) max(x)-min(x)))

# I need to find out if a screen is failing and which. And why.
# Let's start by reproducing the screens, calculating them for each row and comparing their means with the model screen means
mean((ncol(equal_size_sample)-1)-nas_per_row - apply(equal_size_sample, 1, function(x) sum(x==0)))

# coefficient of variation


# kurtosis statistic

# percentage difference

# relative distance

# normalized distance

# number of bidders

# contract value

# sd

# absolute difference