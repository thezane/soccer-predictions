optimizeRatings <- function(tTree, fTree, gTree, gi,
    currentDate, contest) {
  rOptions <- newRatingsOptions(fTree)
  rOutput <- newRatingsOutput(tTree, gTree, gi, currentDate, contest)
  x <- minimizeError(rOptions, rOutput)
  rOutput <- modelRatings(x, rOptions, rOutput)
  rOutput
}

modelRatings <- function(x, rOptions, rOutput, lambda=1) {
  print(x)
  rOptions <- updateOptions(rOptions, x[c(1, 2)], x[3], x[4], x[5],
      x[6], x[7], x[-c(1: 7)])
  rOutput <- rateTeams(rOptions, rOutput)
  strCost <- rOutput$strCost
  strMeanCost <- computeStrMeanCost(rOutput)
  aReg <- lambda * min(0, 1 - 10 * strMeanCost[1]) ^ 2
  dReg <- lambda * min(0, 1 - 10 * strMeanCost[2]) ^ 2
  print(c(strCost, strMeanCost, aReg, dReg))
  rOutput$y <- strCost + aReg + dReg
  rOutput
}

minimizeError <- function(rOptions, rOutput) {
  ks <- c(1, 1)
  c <- 0.5
  aBeta <- 0.5
  dBeta <- 0.6
  corrBeta <- -5
  p <- 0.05
  modelLBd <- c(0.01, 0.01, 0.01, 0, 0, -Inf, 0)
  strFsNorm <- c(-0.2, -0.7, 0.4, -0.3, -0.7, 1)
  strFsNormLBd <- rep(-Inf, length(strFsNorm))
  x <- c(ks, c, aBeta, dBeta, corrBeta, p, strFsNorm)
  xLBd <- c(modelLBd, strFsNormLBd)
  lambda <- 100
  tol <- 0.01
  objFun <- function(x, rOptions.=rOptions, rOutput.=rOutput,
      lambda.=lambda) {
      rOutput <- modelRatings(x, rOptions, rOutput, lambda)
      rOutput$y
  }
  optimObj <- optim(x, objFun, method="L-BFGS-B", lower=xLBd,
      control=list(trace=3, abstol=tol, reltol=tol, REPORT=1))
  x <- optimObj$par
  x
}
