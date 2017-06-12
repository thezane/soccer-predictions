newRatingsCosts <- function(dataset) {
    rCosts <- list(
      dataset=dataset,
      goalsCosts=NULL,
      goalsWeights=NULL,
      strMeanCosts=NULL,
      goalsCost=0,
      strMeanCost=0,
      totalCost=0
    )

    class(rCosts) <- "RatingsCosts"
    rCosts
}

updateGoalsCost.RatingsCosts <- function(rCosts, p, w) {
  rCosts$goalsCosts <- c(rCosts$goalsCosts, p)
  rCosts$goalsWeights <- c(rCosts$goalsWeights, w)
  rCosts
}

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

updateTotalCost <- function(rCosts, rOptions) {
  rCosts$goalsCost <- computeGoalsCost(rCosts)
  rCosts$strMeanCost <- rOptions$strMeanCostReg *
      computeStrMeanCost(rCosts)
  rCosts$totalCost <- rCosts$goalsCost + rCosts$strMeanCost +
      rOptions$slopeCost
  rCosts
}

computeGoalsCost <- function(rCosts) {
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

computeStrMeanCost <- function(rCosts) {
  strMeanCosts <- rCosts$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- 0
  }
  else {
	strMeanCost <- mean(strMeanCosts ^ 2)
  }

  strMeanCost
}
