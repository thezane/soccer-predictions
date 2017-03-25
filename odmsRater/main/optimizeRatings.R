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
  model <- c(
      rOptions$k, rOptions$c,
      rOptions$meanGoals, rOptions$corrBeta,
      rOptions$hA, rOptions$strBeta)
  modelLBd <- c(
      rOptions$kLBd, rOptions$cLBd,
      rOptions$meanGoalsLBd, rOptions$corrBetaLBd,
      rOptions$hALBd, rOptions$strBetaLBd)
  numFs <- rOptions$numFs
  strFsNorm <- c(-0.2, -0.2, 0.2, -0.2, -0.6, 0.6)
  strFsNormLBd <- rep(-Inf, numFs)
  x <- c(model, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  n <- length(x)
  tol <- 0.001
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
      lower=xLBd, control=list(trace=3, maxit=10*n,
      abstol=tol, reltol=tol, pgtol=tol, REPORT=1))
  stopCluster(cluster)
  x <- optimObj$par
  x
}

rateTeams <- function(x, rOptions, rOutput) {
  # Update model parameters
  biasBetas <- x[c(3, 4)]
  featureBetas <- x[c(5, 6)]
  strFsNorm <- x[-c(1: 6)]
  rOptions <- updateOptions(rOptions, x[1], x[2], biasBetas,
      featureBetas, strFsNorm)

  # Compute ratings with updated model
  rOutput <- computeRatings(rOptions, rOutput)

  # Compute regularization
  goalsCost <- 0.01 * computeGoalsCost(rOutput)
  strMeanCost <- computeStrMeanCost(rOutput)
  featureCost <- 0.01 * norm((matrix(featureBetas)), "f")
  fedCost <- 0.01 * norm(matrix(strFsNorm), "f")

  # Compute cost
  strCost <- computeStrCost(rOutput)
  rOutput$y <- strCost + goalsCost + featureCost +
      strMeanCost + fedCost
  rData <- list(rOptions=rOptions, rOutput=rOutput)

  # Print parameters
  printModel(rOptions)

  # Print cost
  print(noquote(sprintf("cost = %f", strCost)))
  print(noquote(sprintf("goals = %f", goalsCost)))
  print(noquote(sprintf("feature = %f", featureCost)))
  print(noquote(sprintf("strMean = %f", strMeanCost)))
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
