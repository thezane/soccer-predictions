optimizeRatings <- function(tTree, fTree, gTree, gi, currentDate) {
  tolRel <- 0.01
  tolScale <- 0.01
  rOptions <- newRatingsOptions(fTree, tolRel, tolScale)
  rOutput <- newRatingsOutput(tTree, gTree, gi, currentDate)
  x <- minimizeError(rOptions, rOutput)
  rOutput <- modelRatings(x, rOptions, rOutput)
  rOutput
}

modelRatings <- function(x, rOptions, rOutput, lambda=1) {
  print(x)
  rOptions <- updateOptions(rOptions, x[1], x[2])
  rOutput <- rateTeams(rOptions, rOutput)
  strCost <- rOutput$strCost
  rOutput$y <- strCost
  rOutput
}

minimizeError <- function(rOptions, rOutput) {
  k <- 0.8
  c <- 0.4
  x <- c(k, c)
  lambda <- 1
  objFun <- function(x, rOptions.=rOptions, rOutput.=rOutput,
      lambda.=lambda) {
      rOutput <- modelRatings(x, rOptions, rOutput, lambda)
      rOutput$y
  }
  optimObj <- optim(x, objFun, method="Nelder-Mead",
      control=list(trace=3, maxit=300, REPORT=1))
  x <- optimObj$par
  x
}
