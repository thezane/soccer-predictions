newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    costs=hash(),
    y=0
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

updateFinalCosts.RatingsOutput <- function(rOutput, rOptions) {
  rOutput$costs["training"] <- updateFinalCosts(
      costs[["training"]], rOptions)
  rOutput$costs["validation"] <- updateFinalCosts(
      costs[["validation"]], rOptions)
  rOutput
}

computeGoalsCosts <- function(rOutput) {
  c(rOutput$costs[["training"]]$goalsCost,
      rOutput$costs[["validation"]]$goalsCost)
}

computeStrMeanCosts <- function(rOutput) {
  c(rOutput$costs[["training"]]$strMeanCost,
      rOutput$costs[["validation"]]$strMeanCost)
}

computeTotalCosts <- function(rOutput) {
  c(rOutput$costs[["training"]]$strTotalCost,
      rOutput$costs[["validation"]]$strTotalCost)
}
