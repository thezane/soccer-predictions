library(parallel)
source("forecastRatings.R")
cores <- detectCores() - 1
cluster <- makeCluster(cores)
dates <- c("6/9/16", "6/14/16", "6/18/16", "6/24/16",
    "6/29/16", "7/5/16", "7/9/16")
parLapply(cluster, dates, forecastRatings)
stopCluster(cluster)
