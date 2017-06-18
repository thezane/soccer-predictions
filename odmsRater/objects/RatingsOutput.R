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
  rOutput$costs[["training"]] <- newRatingsCosts("training")
  rOutput$costs[["validation"]] <- newRatingsCosts("validation")
  rOutput
}

updateGoalsCost.RatingsOutput <- function(rOutput, p, w, dataset) {
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateGoalsCost(rCosts, p, w)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

updateStrMeanCosts.RatingsOutput <- function(rOutput, dataset) {
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateStrMeanCosts(rCosts, rOutput$tTree)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

updateTotalCosts <- function(rOutput, rOptions) {
  rOutput$costs[["training"]] <- updateTotalCost(
      rOutput$costs[["training"]], rOptions)
  rOutput$costs[["validation"]] <- updateTotalCost(
      rOutput$costs[["validation"]], rOptions)
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
  c(rOutput$costs[["training"]]$totalCost,
      rOutput$costs[["validation"]]$totalCost)
}
