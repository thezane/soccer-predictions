optimizeRNN <- function(tTree, fTree, gTree, gi, rData) {
  rOutput <- newRatingsOutput(tTree, gTree, gi)

  if (is.null(rData)) {
    rOptions <- newRatingsOptions(fTree)
	rData <- list(rOptions=rOptions, rOutput=rOutput)
    rData <- trainRNN(rData)
  }
  else {
    rData <- computeRNN(rData)
  }

  rData
}

trainRNN <- function(rData) {
  rOptions <- rData[["rOptions"]]
  x <- getModel(rOptions)
  xLBd <- getModelLBd(rOptions)
  xUBd <- getModelUBd(rOptions)
  n <- length(x)
  fn <- function(x, rData.=rData) {
      rData <- updateRNN(x, rData)
      rOutput <- rData[["rOutput"]]
      rOutput$y
  }
  cores <- min(detectCores() - 1, n)
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(envir=.GlobalEnv), envir=.GlobalEnv)
  gr <- function(x, n.=n, fn.=fn, e=1e-06, cluster.=cluster) {
      computeGradientPar(x, n, fn, e, cluster)
  }
  optimObj <- optim(x, fn, gr, method="L-BFGS-B",
      lower=xLBd, upper=xUBd, control=list(trace=3, lmm=rOptions$lmm,
      factr=rOptions$factr, REPORT=1))
  stopCluster(cluster)
  rData
}

updateRNN <- function(x, rData) {
  rOptions <- rData[["rOptions"]]
  print(rData[["rOutput"]]$y)

  # Update model parameters
  rOptions <- updateOptions(rOptions, x)
  cat("\n")
  printModel(rOptions)

  # Compute RNN with updated parameters
  rData <- computeRNN(rData)
  rOutput <- rData[["rOutput"]]

  # Compute cost
  strMeanCostReg <- rOptions$strMeanCostReg
  slopeCostReg <- rOptions$slopeCostReg
  goalsCost <- computeGoalsCost(rOutput)
  strMeanCost <- strMeanCostReg * computeStrMeanCost(rOutput)
  slopeCost <- slopeCostReg * norm(getModelSlopes(rOptions), "f")
  rOutput$y <- goalsCost[1] + strMeanCost[1] + slopeCost
  rData[["rOutput"]] <- rOutput

  # Print cost
  print(noquote(sprintf("goalsCostT = %f", goalsCost[1])))
  print(noquote(sprintf("strCostT = %f", strMeanCost[1])))
  print(noquote(sprintf("goalsCostV = %f", goalsCost[2])))
  print(noquote(sprintf("strCostV = %f", strMeanCost[2])))
  print(noquote(sprintf("slopeCost = %f", slopeCost)))
  rData
}

computeGradientPar <- function(x, n, f, e, cluster) {
  I <- c(1: n)
  g <- parSapply(cluster, I, function(i, f.=f, x.=x, e.=e) {
      xForDiff <- x
      xForDiff[i] <- xForDiff[i] + e
      xBackDiff <- x
      xBackDiff[i] <- xBackDiff[i] - e
      y <- (f(xForDiff) - f(xBackDiff)) / (2 * e)
      y})
  g
}
