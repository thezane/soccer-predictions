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
    x <- trainRater(rOptions, rOutput)
    rData <- rateTeams(x, rOptions, rOutput)
  }
  else {
    rOptions <- rData[["rOptions"]]
    rOutput <- rateTeams(rOptions, rOutput)
    rData <- list(rOptions=rOptions, rOutput=rOutput)
  }

  rData
}

trainRater <- function(rOptions, rOutput) {
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
      rData <- rateTeams(x, rOptions, rOutput, lambdas)
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
