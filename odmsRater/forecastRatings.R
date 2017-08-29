forecastRatings <- function(rOptions=NULL) {
  library(MASS)
  library(hash)
  library(parallel)
  regexRFiles <- ".*\\.R"
  dataPath <- "../data/"
  srcFiles <- list.files(".", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  
  if (is.null(rOptions)) {
	rOptions = new.RatingsOptions()
  }
  
  readsData <- readData(rOptions, dataPath)
  tTree <- readsData[["tTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  gi <- new.EventIterator(gTree)
  rOutput <- new.RatingsOutput(tTree, gTree, gi)
  
  if (rOptions$isOptimized) {
	rOutput <- computeRNN(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }
  else {
    rData <- list(rOptions=rOptions, rOutput=rOutput)
    rData <- optimizeRNN(tTree, gTree, gi, rData, dataPath)
  }
    
  writeGames(rData, T, dataPath)
  rData
}
