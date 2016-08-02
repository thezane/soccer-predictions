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
  penalty <- 0.01
  penaltyGrowth <- 10
  tolPenalty <- 1
  objFun <- function(x, penalty.=penalty,
      rOptions.=rOptions, rOutput.=rOutput) {
      rOutput <- modelRatings(x, penalty, rOptions, rOutput)
      rOutput$y
  }
  
  while (TRUE) {
    print(x)
    xNext <- fminsearch(objFun, x)
    xDel <- matrix(xNext - x)
    x <- xNext
    
    if (norm(xDel) < tolPenalty) {
      break
    }
    
    penalty <- penaltyGrowth * penalty
  }

  print(x)  
  x
}
