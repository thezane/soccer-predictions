newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    costs=hash()
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

resetRatingsOutput <- function(rOutput) {
  rOutput$costs["training"] <- newRatingsCosts("training")
  rOutput$costs["validation"] <- newRatingsCosts("validation")
  rOutput
}

# Update cost of prediction for goals.
updateGoalsCost.RatingsOutput <- function(rOutput, p, w, dataset) {
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateGoalsCost(rCosts, p, w)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts.RatingsOutput <- function(rOutput, dataset) {
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateStrMeanCosts(rCosts, rOutput$tTree)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

# Compute cost of prediction for goals.
computeGoalsCost.RatingsOutput <- function(rOutput) {
  c(computeGoalsCost(rOutput$costs[["training"]]),
      computeGoalsCost(rOutput$costs[["validation"]]))
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost.RatingsOutput <- function(rOutput) {
  c(computeStrMeanCost(rOutput$costs[["training"]]),
      computeStrMeanCost(rOutput$costs[["validation"]]))
}
