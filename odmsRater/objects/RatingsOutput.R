newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    goalsCosts=NULL,
    goalsWeights=NULL,
    strMeanCosts=NULL
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

# Update cost of prediction for goals.
updateGoalsCost <- function(rOutput, p, w) {
  rOutput$goalsCosts <- c(rOutput$goalsCosts, p)
  rOutput$goalsWeights <- c(rOutput$goalsWeights, 1)
  rOutput
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  strNorms <- data.frame(teams[["strNorm"]])
  aNorms <- unlist(strNorms[1, ])
  dNorms <- unlist(strNorms[2, ])
  strNormMean <- c(mean(aNorms), mean(dNorms)) /
      c(sd(aNorms), sd(dNorms))
  rOutput$strMeanCosts <- c(rOutput$strMeanCosts,
      strNormMean - c(0, 0))
  rOutput
}

# Compute cost of prediction for goals.
computeGoalsCost <- function(rOutput) {
  ps <- rOutput$goalsCosts
  ws <- rOutput$goalsWeights

  if (is.null(ps)) {
    cost <- 0
  }
  else {
    cost <- -log(ps) %*% ws / sum(ws)
  }

  cost
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost <- function(rOutput) {
  strMeanCosts <- rOutput$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- 0
  }
  else {
	strMeanCost <- mean(strMeanCosts ^ 2)
  }

  strMeanCost
}
