constructComputer <- function(rData, iterFile, enableWriteIter) {
  fn <- function(x, rData.=rData, iterFile.=iterFile,
      enableWriteIter.=enableWriteIter) {
    rData <- updateRNN(x, rData)
    rOutput <- rData[["rOutput"]]
    totalCosts <- computeTotalCosts.RatingsOutput(rOutput)
      
    if (enableWriteIter) {
      writeIter(totalCosts[2], x, iterFile)
    }
      
    totalCosts[1]
  }

  fn
}

updateRNN <- function(x, rData) {
  rOptions <- rData[["rOptions"]]
  rOutput <- rData[["rOutput"]]

  # Update model parameters
  rOptions <- update(rOptions, x)
  cat("\n")
  print(rOptions)

  # Compute RNN with updated parameters
  rOutput <- computeRNN(rOptions, rOutput)

  # Compute cost
  goalsCosts <- computeGoalsCosts.RatingsOutput(rOutput)
  slopeCost <- rOptions$slopeCost

  # Print cost
  print(noquote(sprintf("rnnCostT = %f", goalsCosts[1])))
  print(noquote(sprintf("rnnCostV = %f", goalsCosts[2])))
  print(noquote(sprintf("slopeCost = %f", slopeCost)))
  rData[["rOptions"]] <- rOptions
  rData[["rOutput"]] <- rOutput
  rData
}
