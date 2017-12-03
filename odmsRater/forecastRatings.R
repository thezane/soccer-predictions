forecastRatings <- function(rOptions) {
  library(MASS)
  library(hash)
  library(parallel)
  library(skellam)
  regexRFiles <- ".*\\.R"
  inputPath <- "../input/"
  outputPath <- "../output/"
  srcFiles <- list.files(".", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  readsData <- readData(rOptions, inputPath)
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
    rData <- optimizeRNN(tTree, gTree, gi, rData, outputPath)
  }
    
  writeGames(rData, T, outputPath)
  rData
}
