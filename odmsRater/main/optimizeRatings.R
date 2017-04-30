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
      rOptions$c,
      rOptions$kQ,
      rOptions$kT,
      rOptions$meanGoals, rOptions$corrBeta,
      rOptions$hA, rOptions$strBeta)
  modelLBd <- c(
      rOptions$cLBd,
      rOptions$kQLBd,
      rOptions$kTLBd,
      rOptions$meanGoalsLBd, rOptions$corrBetaLBd,
      rOptions$hALBd, rOptions$strBetaLBd)
  modelUBd <- c(
      rOptions$cUBd,
      rOptions$kQUBd,
      rOptions$kTUBd,
      rOptions$meanGoalsUBd, rOptions$corrBetaUBd,
      rOptions$hAUBd, rOptions$strBetaUBd)
  numFs <- rOptions$numFs
  
  # Default federation strengths for Africa, Asia, Europe,
  # North America, Oceania and South America respectively
  strFsNorm <- c(-0.2, -0.2, 0.2, -0.2, -0.6, 0.6)
  strFsNormLBd <- rep(-Inf, numFs)
  strFsNormUBd <- rep(Inf, numFs)
  x <- c(model, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  xUBd <- c(modelUBd, strFsNormUBd)
  n <- length(x)
  tolOpt <- rOptions$tolOpt
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
      lower=xLBd, upper=xUBd, control=list(trace=3, maxit=10*n,
      pgtol=tolOpt, REPORT=1))
  stopCluster(cluster)
  x <- optimObj$par
  x
}

rateTeams <- function(x, rOptions, rOutput) {
  # Update model parameters
  c <- x[1]
  ks <- x[c(2, 3)]
  biasBetas <- x[c(4, 5)]
  featureBetas <- x[c(6, 7)]
  strFsNorm <- x[-c(1: 7)]
  rOptions <- updateOptions(rOptions, c, ks, biasBetas, featureBetas,
      strFsNorm)

  # Print parameters
  cat("\n")
  printModel(rOptions)

  # Compute ratings with updated model
  rOutput <- computeRatings(rOptions, rOutput)

  # Compute regularization
  strMeanCost <- computeStrMeanCost(rOutput)
  fedCost <- 0.01 * norm(as.matrix(strFsNorm), "f")

  # Compute cost
  strCost <- computeStrCost(rOutput)
  rOutput$y <- strCost + strMeanCost + fedCost
  rData <- list(rOptions=rOptions, rOutput=rOutput)

  # Print cost
  print(noquote(sprintf("cost = %f", strCost)))
  print(noquote(sprintf("strMean = %f", strMeanCost)))
  print(noquote(sprintf("fedCost = %f", fedCost)))
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
