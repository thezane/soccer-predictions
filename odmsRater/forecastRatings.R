forecastRatings <- function(rData=NULL) {
  library(MASS)
  library(hash)
  library(parallel)
  regexRFiles <- ".*\\.R"
  dataPath <- "../data/"
  srcFiles <- list.files(".", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  
  if (is.null(rData)) {
	rOptions = new.RatingsOptions()
  }
  else {
    rOptions = rData$rOptions
  }
  
  readsData <- readData(rOptions, dataPath)
  tTree <- readsData[["tTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  gi <- new.EventIterator(gTree)
  rOutput <- new.RatingsOutput(tTree, gTree, gi)
  
  if (is.null(rData)) {
    rData <- list(rOptions=rOptions, rOutput=rOutput)
    rData <- optimizeRNN(tTree, gTree, gi, rData, dataPath)
  }
  else {
    rOutput <- computeRNN(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }
    
  writeGames(rData, T, dataPath)
  rData
}
