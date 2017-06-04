forecastRatings <- function(currentDate, rData=NULL) {
  library(MASS)
  library(hash)
  library(parallel)
  regexRFiles <- ".*\\.R"
  srcFiles <- list.files("../", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  utilitiesFiles <- list.files("../../utilities", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(c(srcFiles, utilitiesFiles), source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  readsData <- readData(currentDate, dateFormat, dataPath)
  tTree <- readsData[["tTree"]]
  fTree <- readsData[["fTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  gi <- newGameIterator(gTree)
  rData <- optimizeRatings(tTree, fTree, gTree, gi, rData)
  writeData(rData, T, dataPath)
  rData
}
