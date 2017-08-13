forecastRatings <- function(currentDate, rData=NULL) {
  library(MASS)
  library(hash)
  library(parallel)
  regexRFiles <- ".*\\.R"
  dataPath <- "../data/"
  srcFiles <- list.files(".", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  dateFormat <- "%m/%d/%y"
  currentDate <- as.Date(currentDate, dateFormat)
  readsData <- readData(currentDate, dateFormat, dataPath)
  tTree <- readsData[["tTree"]]
  fTree <- readsData[["fTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  gi <- newEventIterator(gTree)
  rOutput <- newRatingsOutput(tTree, gTree, gi)
  
  if (is.null(rData)) {
    rData <- optimizeRNN(tTree, fTree, gTree, gi, rOutput, dataPath)
  }
  else {
    rOutput <- computeRNN(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }
    
  writeGames(rData, T, dataPath)
  rData
}
