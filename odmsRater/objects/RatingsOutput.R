newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    costs=hash()
  )
  
  rOutput$costs["training"] <- newRatingsCosts("training")
  rOutput$costs["validation"] <- newRatingsCosts("validation")
  
  class(rOutput) <- "RatingsOutput"
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
  rCostsTraining <- rOutput$costs[["training"]]
  rCostsValidation <- rOutput$costs[["validation"]]
  c(computeGoalsCost(rCostsTraining),
      computeGoalsCost(rCostsValidation))
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost.RatingsOutput <- function(rOutput) {
  rCostsTraining <- rOutput$costs[["training"]]
  rCostsValidation <- rOutput$costs[["validation"]]
  c(computeStrMeanCost(rCostsTraining),
      computeStrMeanCost(rCostsValidation))
}
