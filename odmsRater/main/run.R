run <- function() {
  source("forecastRatings.R")
  source("../verifier/verifyOdms.R")
  load("../../data/odms-2014-07-13.RData")
  library(parallel)
  dates <- c("6/9/16", "6/14/16", "6/18/16", "6/24/16",
      "6/29/16", "7/5/16", "7/9/16")
  contest = "EUC"
  cores <- detectCores() - 1
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(envir=.GlobalEnv), envir=.GlobalEnv)
  parLapply(cluster, dates, function (currentDate,
      contest.=contest, rData.=rData) {
      forecastRatings(currentDate, contest, rData)})
  stopCluster(cluster)
  verifyOdms(2016, contest, "France")
}
