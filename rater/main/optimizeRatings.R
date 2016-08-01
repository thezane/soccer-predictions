optimizeRatings <- function(tTree, fTree, gTree, gi, numMatches) {
  tolRel <- 1e-04
  tolScale <- 1e-04
  rOptions <- newRatingsOptions(fTree, tolRel, tolScale)
  rOutput <- newRatingsOutput(tTree, gTree, gi, numMatches)
  x <- minimize(rOptions, rOutput)
  rOutput <- modelRatings(x, 0, rOptions, rOutput)
  rOutput
}

modelRatings <- function(x, penalty, rOptions, rOutput) {
  rOptions <- updateOptions(rOptions, x[1], x[2], x[-c(1, 2)])
  rOutput <- rateTeams(rOptions, rOutput)
  strCost <- rOutput$strCost
  strMedianCost <- computeStrMedianCost(rOutput)
  medianConstraint <- min(0, 1 - 50 * strMedianCost)
  rOutput$y <- strCost + penalty * medianConstraint ^ 2
  rOutput
}

minimize <- function(rOptions, rOutput) {
  k <- 1
  c <- 0.5
  strFs <- matrix(1, 1, rOptions$numFs)
  x <- c(k, c, strFs)
  penalty <- 1
  penaltyGrowth <- 10
  tolPenalty <- 0.01
  
  while (TRUE) {
    optimData <- optim(x, objFun, penalty=penalty, rOptions=rOptions,
        rOutput=rOutput, method="Nelder-Mead", control=list(trace=TRUE))
    xNext <- optimData[["par"]]
    xDel <- matrix(xNext - x)
    x <- xNext
    print(x)
    
    if (norm(xDel) < tolPenalty) {
      break
    }
    
    penalty <- penaltyGrowth * penalty
  }
  
  x
}

objFun <- function(x, penalty, rOptions, rOutput) {
  rOutput <- modelRatings(x, penalty, rOptions, rOutput)
  rOutput$y
}
