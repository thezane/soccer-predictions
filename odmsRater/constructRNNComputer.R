constructRNNComputer <- function(rData, iterFile, enableWriteIter) {
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

updateRNN <- function(x, rData) {
  rOptions <- rData[["rOptions"]]
  rOutput <- rData[["rOutput"]]

  # Update model parameters
  rOptions <- updateOptions(rOptions, x)
  cat("\n")
  printModel(rOptions)

  # Compute RNN with updated parameters
  rOutput <- computeRNN(rOptions, rOutput)

  # Compute cost
  goalsCosts <- computeGoalsCosts(rOutput)
  strMeanCosts <- computeStrMeanCosts(rOutput)
  slopeCost <- rOptions$slopeCost

  # Print cost
  print(noquote(sprintf("goalsCostT = %f", goalsCosts[1])))
  print(noquote(sprintf("strCostT = %f", strMeanCosts[1])))
  print(noquote(sprintf("goalsCostV = %f", goalsCosts[2])))
  print(noquote(sprintf("strCostV = %f", strMeanCosts[2])))
  print(noquote(sprintf("slopeCost = %f", slopeCost)))
  rData[["rOptions"]] <- rOptions
  rData[["rOutput"]] <- rOutput
  rData
}
