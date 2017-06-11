newRatingsCosts <- function(dataset) {
    rCosts <- list(
      dataset=dataset,
      goalsCosts=NULL,
      goalsWeights=NULL,
      strMeanCosts=NULL
    )

    class(rCosts) <- "RatingsCosts"
    rCosts
}

# Update cost of prediction for goals.
updateGoalsCost.RatingsCosts <- function(rCosts, p, w) {
  rCosts$goalsCosts <- c(rCosts$goalsCosts, p)
  rCosts$goalsWeights <- c(rCosts$goalsWeights, w)
  rCosts
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts.RatingsCosts <- function(rCosts, tTree) {
  teams <- data.frame(t(values(tTree)))
  strNorms <- data.frame(teams[["strNorm"]])
  aNorms <- unlist(strNorms[1, ])
  dNorms <- unlist(strNorms[2, ])
  strNormMean <- c(mean(aNorms), mean(dNorms)) /
      c(sd(aNorms), sd(dNorms))
  rCosts$strMeanCosts <- c(rCosts$strMeanCosts,
      strNormMean - c(0, 0))
  rCosts
}

# Compute cost of prediction for goals.
computeGoalsCost.RatingsCosts <- function(rCosts) {
  ps <- rCosts$goalsCosts
  ws <- rCosts$goalsWeights

  if (is.null(ps)) {
    cost <- 0
  }
  else {
    cost <- -log(ps) %*% ws / sum(ws)
  }

  cost
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost.RatingsCosts <- function(rCosts) {
  strMeanCosts <- rCosts$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- 0
  }
  else {
	strMeanCost <- mean(strMeanCosts ^ 2)
  }

  strMeanCost
}
