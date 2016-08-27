optimizeRatings <- function(tTree, fTree, gTree, gi,
    currentDate, contest) {
  rOptions <- newRatingsOptions(fTree)
  rOutput <- newRatingsOutput(tTree, gTree, gi, currentDate, contest)
  x <- c(2.59289653,  0.01000000,  0.48831919,  0.04059758,  0.94224021, -5.00000081,  0.15194786, -0.03565156, -1.54143405,  0.76698815,  0.47116043, -1.60973200, 1.86555621)
  #x <- minimizeError(rOptions, rOutput)
  rData <- modelRatings(x, rOptions, rOutput)
  rData
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
  lambda <- 100
  tol <- 0.01
  objFun <- function(x, rOptions.=rOptions, rOutput.=rOutput,
      lambda.=lambda) {
      rData <- modelRatings(x, rOptions, rOutput, lambda)
      rOutput <- rData[["rOutput"]]
      rOutput$y
  }
  optimObj <- optim(x, objFun, method="L-BFGS-B", lower=xLBd,
      control=list(trace=3, maxit=4*n,
      abstol=tol, reltol=tol, pgtol=tol, REPORT=1))
  x <- optimObj$par
  x
}
