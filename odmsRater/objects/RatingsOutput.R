new.RatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    tTreeBackup=tTree,
    gTree=gTree,
    gi=gi,
    costs=hash(),
    y=0
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

reset.RatingsOutput <- function(rOutput) {
  rOutput$costs[["training"]] <- new.RatingsCosts("training")
  rOutput$costs[["validation"]] <- new.RatingsCosts("validation")
  rOutput$tTree <- copyTTree.RatingsOutput(rOutput,
      rOutput$tTreeBackup)
  rOutput
}

copyTTree.RatingsOutput <- function(rOutput, tTree) {
  tTreeCopy <- hash()
  teams <- keys(tTree)
  n <- length(teams)
  i <- 1

  while (i <= n) {
    teamName <- teams[i]
    team <- tTree[[teamName]]
    tTreeCopy[[teamName]] <- newTeam(teamName, team$fName)
    i <- i + 1
  }

  tTreeCopy
}

updateGoalsCost.RatingsOutput <- function(rOutput, p, w, dataset) {
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateGoalsCost.RatingsCosts(rCosts, p, w)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

updateStrMeanCosts.RatingsOutput <- function(rOutput, dataset) {
  rCosts <- rOutput$costs[[dataset]]
  rCosts <- updateStrMeanCosts.RatingsCosts(rCosts, rOutput$tTree)
  rOutput$costs[dataset] <- rCosts
  rOutput
}

updateTotalCosts.RatingsOutput <- function(rOutput, rOptions) {
  rOutput$costs[["training"]] <- updateTotalCost.RatingsCosts(
      rOutput$costs[["training"]], rOptions)
  rOutput$costs[["validation"]] <- updateTotalCost.RatingsCosts(
      rOutput$costs[["validation"]], rOptions)
  rOutput
}

computeGoalsCosts.RatingsOutput <- function(rOutput) {
  c(rOutput$costs[["training"]]$goalsCost,
      rOutput$costs[["validation"]]$goalsCost)
}

computeStrMeanCosts.RatingsOutput <- function(rOutput) {
  c(rOutput$costs[["training"]]$strMeanCost,
      rOutput$costs[["validation"]]$strMeanCost)
}

computeTotalCosts.RatingsOutput <- function(rOutput) {
  c(rOutput$costs[["training"]]$totalCost,
      rOutput$costs[["validation"]]$totalCost)
}
