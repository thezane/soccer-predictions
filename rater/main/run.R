run <- function() {
  source("forecastRatings.R")
  source("../verifier/verifySodm.R")
  load("../../data/sodm-2014-07-13.Rdata")
  library(parallel)
  dates <- c("6/9/16", "6/14/16", "6/18/16", "6/24/16",
      "6/29/16", "7/5/16", "7/9/16")
  currentContest = "EUC"
  cores <- detectCores() - 1
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(envir=.GlobalEnv), envir=.GlobalEnv)
  parLapply(cluster, dates, function (currentDate,
      currentContest.=currentContest, rData.=rData) {
      forecastRatings(currentDate, currentContest, rData)})
  stopCluster(cluster)
  verifySodm(2016, "France")
}
