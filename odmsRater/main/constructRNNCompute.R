constructRNNCompute <- function(rData, iterFile, enableWriteIter) {
  fn <- function(x, rData.=rData, iterFile.=iterFile,
      enableWriteIter.=enableWriteIter) {
    rData <- updateRNN(x, rData)
    rOutput <- rData[["rOutput"]]
    totalCosts <- computeTotalCosts(rOutput)
      
    if (enableWriteIter) {
      writeIter(totalCosts[2], x, iterFile)
    }
      
    totalCosts[1]
  }

  fn
}
