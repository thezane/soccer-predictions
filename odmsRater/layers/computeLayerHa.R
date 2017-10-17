computeLayerHa <- function(game, rOptions) {
  meanGoalsData <- list()
  meanGoals <- rOptions$meanGoals * c(1, 1)
  goalsOdmHa <- game$goalsOdm

  if (game$existsHa) {
    homeMeanGoals <- rOptions$meanGoals + rOptions$haBias
    awayMeanGoals <- rOptions$meanGoals
    meanGoals[1] <- homeMeanGoals
    goalsOdmHa[1] <- (awayMeanGoals / homeMeanGoals) * goalsOdmHa[1]
  }

  A <- matrix(c(0, goalsOdmHa[2], goalsOdmHa[1], 0), 2, 2, TRUE)
  meanGoalsData <- list()
  meanGoalsData[["A"]] <- A
  meanGoalsData[["meanGoals"]] <- meanGoals
  meanGoalsData
}
