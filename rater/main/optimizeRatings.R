optimizeRatings <- function(tTree, fTree, optPrereqs, relevantGoals,
    rData) {
  gTree <- optPrereqs[["gTree"]]
  gi <- optPrereqs[["gi"]]
  goalsRelevant <- optPrereqs[["goalsRelevant"]]
  meanGoalsMap <- optPrereqs[["meanGoalsMap"]]
  rOutput <- newRatingsOutput(tTree, gTree, gi, goalsRelevant,
      meanGoalsMap)

  if (is.null(rData)) {
    rOptions <- newRatingsOptions(fTree)
    x <- minimizeError(rOptions, rOutput)
    rData <- modelRatings(x, rOptions, rOutput)
  }
  else {
    rOptions <- rData[["rOptions"]]
    rOutput <- rateTeams(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }

  rData
}

modelRatings <- function(x, rOptions, rOutput, lambdas=rep(1, 4)) {
  print(x)
  strFsNorm <- x[-c(1: 6)]
  rOptions <- updateOptions(rOptions, x[c(1, 2)], x[3], x[c(4, 5)],
      x[6], strFsNorm)
  rOutput <- rateTeams(rOptions, rOutput)
  strCost <- rOutput$strCost
  goalsCost <- computeGoalsCost(rOutput)
  strMeanCost <- computeStrMeanCost(rOutput)
  strFsNormCost <- norm(matrix(strFsNorm), "f")
  xpCost <- rOutput$xpCost
  goalsReg <- lambdas[1] * min(0, 1.1 - goalsCost) ^ 2
  strReg <- lambdas[2] * (min(0, 0.05 - strMeanCost[1]) ^ 2 +
      min(0, 0.05 - strMeanCost[1]) ^ 2)
  strFsNormReg <- lambdas[3] * min(0, 1 - strFsNormCost) ^ 2
  xpReg <- lambdas[4] * xpCost
  print(c(strCost, goalsReg, strReg, strFsNormReg, xpCost))
  rOutput$y <- strCost + goalsReg + strReg + strFsNormReg + xpReg
  rData <- list(rOptions=rOptions, rOutput=rOutput)
  rData
}

minimizeError <- function(rOptions, rOutput) {
  model <- c(1.5, 1.5, 0.3, 0.5, 0.5, -3)
  modelLBd <- c(0, 0, 0.01, 0, 0, -Inf)
  numFs <- rOptions$numFs
  strFsNorm <- c(-0.2, -0.6, 0.2, -0.2, -0.6, 0.6)
  strFsNormLBd <- rep(-Inf, numFs)
  x <- c(model, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  n <- length(x)
  lambdas <- c(100, 1e+03, 100, 100)
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
  optimObj <- optim(x, fn, gr, method="L-BFGS-B",
      lower=xLBd, control=list(trace=3, maxit=4*n,
      abstol=tol, reltol=tol, pgtol=tol, REPORT=1))
  stopCluster(cluster)
  x <- optimObj$par
  x
}
