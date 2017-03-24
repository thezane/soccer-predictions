newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    strCosts=NULL,
    goalsCosts=NULL,
    strMeanCosts=NULL,
    y=Inf
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

# Update cost of prediction.
updateStrCost <- function(rOutput, expectedResult, actualResult) {
  rOutput$strCosts = c(rOutput$strCosts,
      computeSSE(expectedResult, actualResult))
  rOutput
}

# Update distance of expected goals from actual goals.
updateGoalsCost <- function(rOutput, goalsExpected, goalsActual) {
  rOutput$goalsCosts <- c(rOutput$goalsCosts,
      computeSSE(goalsExpected, goalsActual))
  rOutput
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  strNorms <- data.frame(teams[["strNorm"]])
  strNormMean <- c(mean(strNorms[[1]]), mean(strNorms[[2]]))
  strMeanCost <- computeSSE(strNormMean, c(0, 0))
  rOutput$strMeanCosts <- c(rOutput$strMeanCosts, strMeanCost)
  rOutput
}

# Compute cost of prediction.
computeStrCost <- function(rOutput) {
  strCosts <- rOutput$strCosts

  if (is.null(strCosts)) {
    strCost <- 0
  }
  else {
    strCost <- mean(strCosts)
  }

  strCost
}

# Compute distance of expected goals from actual goals.
computeGoalsCost <- function(rOutput) {
  goalsCosts <- rOutput$goalsCost

  if (is.null(goalsCosts)) {
    goalsCost <- 0
  }
  else {
    goalsCost <- mean(goalsCosts)
  }

  goalsCost
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost <- function(rOutput) {
  strMeanCosts <- rOutput$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- 0
  }
  else {
	strMeanCostsMat <- matrix(strMeanCosts, ncol=2, byrow=TRUE)
    strMeanCost <- sum(mean(strMeanCostsMat[, 1]),
        mean(strMeanCostsMat[, 2]))
  }

  strMeanCost
}
