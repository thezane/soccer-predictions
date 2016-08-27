run <- function() {
  library(parallel)
  dates <- c("6/9/16", "6/14/16", "6/18/16", "6/24/16",
      "6/29/16", "7/5/16", "7/9/16")
  contest = "EUC"
  cores <- detectCores() - 1
  cluster <- makeCluster(cores)
  parLapply(cluster, dates, function (currentDate, contest.=contest) {
	  source("forecastRatings.R")
      forecastRatings(currentDate, contest)})
  stopCluster(cluster)
}
