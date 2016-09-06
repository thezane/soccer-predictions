forecastRatings <- function(currentDate, currentContest, rData=NULL) {
  library(hash)
  library(parallel)
  library(MASS)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  readsData <- readData(currentDate, currentContest,
      dateFormat, dataPath)
  tTree <- readsData[["tTree"]]
  fTree <- readsData[["fTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  gi <- newGameIterator(gTree)
  optPrereqs <- computeOptPrereqs(gTree, gi, T)
  rData <- optimizeRatings(tTree, fTree, optPrereqs, relevantGoals,
      rData)
  writeData(rData, T, dataPath)
  rData
}
