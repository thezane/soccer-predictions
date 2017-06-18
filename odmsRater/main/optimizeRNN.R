optimizeRNN <- function(tTree, fTree, gTree, gi, rData, dataPath) {
  rOutput <- newRatingsOutput(tTree, gTree, gi)

  if (is.null(rData)) {
    rOptions <- newRatingsOptions(fTree)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
    x <- trainRNN(rData, dataPath)
    rData <- updateRNN(x, rData)
  }
  else {
    rOptions <- rData[["rOptions"]]
    rOutput <- computeRNN(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }

  rData
}

trainRNN <- function(rData, dataPath) {
  rOptions <- rData[["rOptions"]]
  rOutput <- rData[["rOutput"]]
  x <- getModel(rOptions)
  xLBd <- getModelLBd(rOptions)
  xUBd <- getModelUBd(rOptions)
  iterFile <- paste(dataPath, "odms-iters.csv", sep="")
  
  if (file.exists(iterFile)) {
    file.remove(iterFile)
  }

  fn <- constructRNNCompute(rData, iterFile, TRUE)
  fi <- constructRNNCompute(rData, iterFile, FALSE)
  
  fi(x)
  
  cores <- min(detectCores() - 1, n)
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(envir=.GlobalEnv), envir=.GlobalEnv)
  gr <- function(x, n.=n, fn.=fn, e=1e-06, cluster.=cluster) {
      computeGradientPar(x, n, fi, e, cluster)
  }
  optimObj <- optim(x, fn, gr, method="L-BFGS-B",
      lower=xLBd, upper=xUBd, control=list(trace=3, lmm=rOptions$lmm,
      factr=rOptions$factr, REPORT=1))
  stopCluster(cluster)
  x <- readIter(iterFile)
  x
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
