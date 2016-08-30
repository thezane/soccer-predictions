optimizeRatings <- function(tTree, fTree, optPrereqs, relevantGoals,
    rData) {
  gTree <- optPrereqs[["gTree"]]
  gi <- optPrereqs[["gi"]]
  hA <- optPrereqs[["hA"]]
  relevantGoals <- optPrereqs[["goalsRelevant"]]
  rOutput <- newRatingsOutput(tTree, gTree, gi, hA, relevantGoals)

  if (is.null(rData)) {
    rOptions <- newRatingsOptions(fTree)
    x <- minimizeError(rOptions, rOutput)
  }
  else {
    rOptions <- rData[["rOptions"]]
    x <- convertToX(rData[["rOptions"]])
  }

  rData <- modelRatings(x, rOptions, rOutput)
  rData
}

modelRatings <- function(x, rOptions, rOutput, lambdas=c(1, 1)) {
  print(x)
  strFsNorm <- x[-c(1: 4)]
  rOptions <- updateOptions(rOptions, x[c(1, 2)], x[3], x[4],
      strFsNorm)
  rOutput <- rateTeams(rOptions, rOutput)
  strCost <- rOutput$strCost
  goalsCost <- computeGoalsCost(rOutput)
  strMeanCost <- computeStrMeanCost(rOutput)
  goalsReg <- lambdas[1] * min(0, 1.1 - goalsCost) ^ 2
  strFsNormCost <- 
  aReg <- lambdas[2] * min(0, 1 - 20 * strMeanCost[1]) ^ 2
  dReg <- lambdas[2] * min(0, 1 - 20 * strMeanCost[2]) ^ 2
  print(c(strCost, goalsCost, strMeanCost, goalsReg, aReg, dReg))
  rOutput$y <- strCost + goalsReg + aReg + dReg
  rData <- list(rOptions=rOptions, rOutput=rOutput)
  rData
}

minimizeError <- function(rOptions, rOutput) {
  model <- c(1, 1, 0.3, 0.4)
  modelLBd <- c(0.1, 0.1, 0.1, 0)
  numFs <- rOptions$numFs
  strFsNorm <- c(-0.2, -0.6, 0.2, -0.2, -0.6, 0.6)
  strFsNormLBd <- rep(-Inf, numFs)
  x <- c(model, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  n <- length(x)
  lambdas <- c(100, 1e+03)
  tol <- 0.01
  fn <- function(x, rOptions.=rOptions, rOutput.=rOutput,
      lambdas.=lambdas) {
      rData <- modelRatings(x, rOptions, rOutput, lambdas)
      rOutput <- rData[["rOutput"]]
      rOutput$y
  }
  cores <- min(detectCores() - 1, n)
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(envir=.GlobalEnv), envir=.GlobalEnv)
  gr <- function(x, n.=n, fn.=fn, e=1e-03, cluster.=cluster) {
      computeGradientPar(x, n, fn, e, cluster)
  }
  optimObj <- optim(x, fn, gr, method="L-BFGS-B", lower=xLBd,
      control=list(trace=3, maxit=5*n,
      abstol=tol, reltol=tol, pgtol=tol, REPORT=1))
  stopCluster(cluster)
  x <- optimObj$par
  x
}
