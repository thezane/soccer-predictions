new.RatingsCosts <- function(dataset) {
    rCosts <- list(
      dataset=dataset,
      goalsCosts=NULL,
      goalsWeights=NULL,
      goalsCost=0,
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

updateTotalCost.RatingsCosts <- function(rCosts, rOptions) {
  rCosts$goalsCost <- computeGoalsCost.RatingsCosts(rCosts)
  rCosts$strMeanCost <- rOptions$strMeanCostReg *
      computeStrMeanCost.RatingsCosts(rCosts)
  rCosts$totalCost <- rCosts$goalsCost + rCosts$strMeanCost +
      rOptions$slopeCost
  rCosts
}

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
