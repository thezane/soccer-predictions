newRatingsOutput <- function(tTree, gTree, gi, numMatches) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    strCost=0,
    n=numMatches,
    strAll=matrix(0, numMatches, 2),
    startI=1,
    endI=2,
    y=Inf
  )
  class(rOutput) <- "RatingsOutput"
  rOutput
}

updateStrAll <- function(rOutput, teamStr) {
  if (rOutput$endI <= rOutput$n) {
    rOutput$strAll[rOutput$startI: rOutput$endI,] <- teamStr
    rOutput$startI <- rOutput$startI + 2
    rOutput$endI <- rOutput$endI + 2
  }
  
  rOutput
}

computeStrMedianCost <- function(rOutput) {
  strMedian <- median(rOutput$strAll)
  strMedianCost <-
      computeMSE(computeStrNorm(strMedian[1]), 0) +
      computeMSE(computeStrNorm(strMedian[2]), 0)
  strMedianCost
}
