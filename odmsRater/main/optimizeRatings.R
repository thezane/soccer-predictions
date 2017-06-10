optimizeRatings <- function(tTree, fTree, gTree, gi, rData) {
  rOutput <- newRatingsOutput(tTree, gTree, gi)

  if (is.null(rData)) {
    rOptions <- newRatingsOptions(fTree)
    x <- trainRater(rOptions, rOutput)
    rData <- rateTeams(x, rOptions, rOutput)
  }
  else {
    rOptions <- rData[["rOptions"]]
    rOutput <- computeRatings(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }

  rData
}

trainRater <- function(rOptions, rOutput) {
  x <- getModel(rOptions)
  xLBd <- getModelLBd(rOptions)
  xUBd <- getModelUBd(rOptions)
  n <- length(x)
  fn <- function(x, rOptions.=rOptions, rOutput.=rOutput) {
      rData <- rateTeams(x, rOptions, rOutput)
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
  x <- optimObj$par
  x
}

rateTeams <- function(x, rOptions, rOutput) {
  # Update model parameters
  rOptions <- updateOptions(rOptions, x)
  cat("\n")
  printModel(rOptions)

  # Compute ratings with updated model
  rOutput <- computeRatings(rOptions, rOutput)

  # Compute cost
  goalsCost <- computeGoalsCost(rOutput)
  slopeCost <- 0.001 * norm(getModelSlopes(rOptions), "f")
  strMeanCost <- 0.1 * computeStrMeanCost(rOutput)
  rOutput$y <- goalsCost + slopeCost + strMeanCost
  rData <- list(rOptions=rOptions, rOutput=rOutput)

  # Print cost
  print(noquote(sprintf("goalscost = %f", goalsCost)))
  print(noquote(sprintf("slopeCost = %f", slopeCost)))
  print(noquote(sprintf("strMeanCost = %f", strMeanCost)))
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
