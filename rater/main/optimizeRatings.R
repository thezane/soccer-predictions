optimizeRatings <- function(tTree, fTree, gTree, gi, hA, relevantGoals,
    currentDate) {
  rOptions <- newRatingsOptions(fTree)
  rOutput <- newRatingsOutput(tTree, gTree, gi, hA, relevantGoals,
      currentDate)
  x <- minimizeError(rOptions, rOutput)
  rData <- modelRatings(x, rOptions, rOutput)
  rData
}

modelRatings <- function(x, rOptions, rOutput, lambdas=c(1, 1)) {
  print(x)
  rOptions <- updateOptions(rOptions, x[c(1, 2)], x[3], x[4], x[5],
      x[6], x[7], x[-c(1: 7)])
  rOutput <- rateTeams(rOptions, rOutput)
  strCost <- rOutput$strCost
  goalsCost <- computeGoalsCost(rOutput)
  strMeanCost <- computeStrMeanCost(rOutput)
  goalsReg <- lambdas[1] * min(0, 1.1 - goalsCost) ^ 2
  aReg <- lambdas[2] * min(0, 1 - 10 * strMeanCost[1]) ^ 2
  dReg <- lambdas[2] * min(0, 1 - 10 * strMeanCost[2]) ^ 2
  print(c(strCost, goalsCost, strMeanCost, goalsReg, aReg, dReg))
  rOutput$y <- strCost + goalsReg + aReg + dReg
  rData <- list(rOptions=rOptions, rOutput=rOutput)
  rData
}

minimizeError <- function(rOptions, rOutput) {
  ks <- c(1, 1)
  c <- 0.5
  aBeta <- 0.5
  dBeta <- 0.5
  corrBeta <- -5
  p <- 0.05
  modelLBd <- c(0.01, 0.01, 0.01, 0, 0, -Inf, 0)
  strFsNorm <- c(-0.2, -0.7, 0.4, -0.3, -0.7, 1)
  strFsNormLBd <- rep(-Inf, length(strFsNorm))
  x <- c(ks, c, aBeta, dBeta, corrBeta, p, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  n <- length(x)
  lambdas <- c(100, 100)
  tol <- 0.01
  objFun <- function(x, rOptions.=rOptions, rOutput.=rOutput,
      lambdas.=lambdas) {
      rData <- modelRatings(x, rOptions, rOutput, lambdas)
      rOutput <- rData[["rOutput"]]
      rOutput$y
  }
  optimObj <- optim(x, objFun, method="L-BFGS-B", lower=xLBd,
      control=list(trace=3, maxit=2*n,
      abstol=tol, reltol=tol, pgtol=tol, REPORT=1))
  x <- optimObj$par
  x
}
