optimizeRatings <- function(tTree, fTree, optPrereqs, rData) {
  gTree <- optPrereqs[["gTree"]]
  gi <- optPrereqs[["gi"]]
  meanGoalsMap <- optPrereqs[["meanGoalsMap"]]
  rOutput <- newRatingsOutput(tTree, gTree, gi, meanGoalsMap)

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
  model <- c(1, 0.3, 0.5, 0.5, -1)
  modelLBd <- c(0, 0.01, 0, 0, -Inf)
  numFs <- rOptions$numFs
  strFsNorm <- c(-0.2, -0.6, 0.2, -0.2, -0.6, 0.6)
  strFsNormLBd <- rep(-Inf, numFs)
  x <- c(model, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  n <- length(x)
  tol <- 0.01
  fn <- function(x, rOptions.=rOptions, rOutput.=rOutput) {
      rData <- rateTeams(x, rOptions, rOutput)
      rOutput <- rData[["rOutput"]]
      rOutput$y
  }
  cores <- min(detectCores() - 1, n)
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(envir=.GlobalEnv), envir=.GlobalEnv)
  gr <- function(x, n.=n, fn.=fn, e=1e-03, cluster.=cluster) {
      computeGradientPar(x, n, fn, e, cluster)
  }
  optimObj <- optim(x, fn, gr, method="L-BFGS-B",
      lower=xLBd, control=list(trace=3, maxit=4*n,
      abstol=tol, reltol=tol, pgtol=tol, REPORT=1))
  stopCluster(cluster)
  x <- optimObj$par
  x
}

rateTeams <- function(x, rOptions, rOutput) {
  # Update model parameters
  strFsNorm <- x[-c(1: 5)]
  rOptions <- updateOptions(rOptions, x[1], x[2], x[c(3, 4)],
      x[5], strFsNorm)

  # Compute ratings with updated model
  rOutput <- computeRatings(rOptions, rOutput)

  # Compute regularization
  goalsCost <- 0.1 * computeGoalsCost(rOutput)
  strMeanCost <- 0.1 * computeStrMeanCost(rOutput)
  fedCost <- 0.1 * norm(matrix(strFsNorm), "f")

  # Compute cost
  strCost <- computeStrCost(rOutput)
  rOutput$y <- strCost + goalsCost + strMeanCost + fedCost
  rData <- list(rOptions=rOptions, rOutput=rOutput)

  # Print parameters
  printModel(rOptions)

  # Print cost
  print(noquote(sprintf("cost = %f", strCost)))
  print(noquote(sprintf("goals = %f", goalsCost)))
  print(noquote(sprintf("str = %f", strMeanCost)))
  print(noquote(sprintf("fed = %f", fedCost)))
  cat("\n")
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
