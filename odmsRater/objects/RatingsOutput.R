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
updateGoalsCost.RatingsOutput <- function(rOutput, p, w, isTraining) {
  dataset <- getDataset(rOutput, isTraining)
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateGoalsCost(rCosts, p, w)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts.RatingsOutput <- function(rOutput, isTraining) {
  dataset <- getDataset(rOutput, isTraining)
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateStrMeanCosts(rCosts, rOutput$tTree)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

# Compute cost of prediction for goals.
computeGoalsCost.RatingsOutput <- function(rOutput, isTraining) {
  dataset <- getDataset(rOutput, isTraining)
  rCosts <- rOutput$costs[[dataset]]
  computeGoalsCost(rCosts)
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost.RatingsOutput <- function(rOutput, isTraining) {
  dataset <- getDataset(rOutput, isTraining)
  rCosts <- rOutput$costs[[dataset]]
  computeStrMeanCost(rCosts)
}

getDataset <- function(rOutput, isTraining) {
  if (isTraining) {
    "training"
  }
  else {
    "validation"
  }
}
