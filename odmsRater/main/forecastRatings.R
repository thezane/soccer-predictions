forecastRatings <- function(currentDate, currentContest="WOC",
    rData=NULL) {
  library(hash)
  library(parallel)
  library(MASS)
  regexRFiles <- ".*\\.R"
  srcFiles <- list.files("../", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  utilitiesFiles <- list.files("../../utilities", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(c(srcFiles, utilitiesFiles), source)
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
  optPrereqs <- constructOptPrereqs(gTree, gi, tTree, T)
  rData <- optimizeRatings(tTree, fTree, optPrereqs, rData)
  writeData(rData, T, dataPath)
  rData
}
