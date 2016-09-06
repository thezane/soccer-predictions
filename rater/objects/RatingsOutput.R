newRatingsOutput <- function(tTree, gTree, gi, goalsRelevant,
    meanGoalsMap) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    goalsRelevant=goalsRelevant,
    goalsExpected=0,
    kCost=1/(4*365),
    meanGoalsMap=meanGoalsMap,
    strCosts=0,
    strMeanCosts=NULL,
    y=Inf
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

updateStrCosts <- function(rOutput, goalsExpected, strCosts, game) {
  rOutput$strCosts <- c(rOutput$strCosts, strCosts)
  rOutput$goalsExpected <- rOutput$goalsExpected + sum(goalsExpected)
  rOutput
}

updateStrMeanCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  teamStrNorms <- computeStrNorm(data.frame(teams[["teamStr"]]))
  strNormMean <- c(mean(teamStrNorms[[1]]),
      mean(teamStrNorms[[2]]))
  strMeanCost <- c(computeMSE(strNormMean[1], 0),
      computeMSE(strNormMean[2], 0))
  rOutput$strMeanCosts <- c(rOutput$strMeanCosts, strMeanCost)
  rOutput
}

computeGoalsCost <- function(rOutput) {
  goalsExpected <- rOutput$goalsExpected
  goalsRelevant <- rOutput$goalsRelevant
  goalsCost <- max(c(goalsExpected / goalsRelevant,
      goalsRelevant / goalsExpected))
}

computeStrMeanCost <- function(rOutput) {
  strMeanCosts <- rOutput$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- c(0, 0)
  }
  else {
	strMeanCostsMat <- matrix(strMeanCosts, ncol=2, byrow=TRUE)
    strMeanCost <- c(mean(strMeanCostsMat[, 1]),
        mean(strMeanCostsMat[, 2]))
  }

  strMeanCost
}
