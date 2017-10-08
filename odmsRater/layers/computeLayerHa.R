computeLayerHa <- function(game, rOptions) {
  meanGoalsData <- list()
  meanGoals <- rOptions$meanGoals * c(1, 1)
  goalsNorm <- game$goals

  if (game$existsHa) {
    homeMeanGoals <- rOptions$meanGoals + rOptions$haBias
    awayMeanGoals <- rOptions$meanGoals
    meanGoals[1] <- homeMeanGoals
    goalsNorm[1] <- (awayMeanGoals / homeMeanGoals) * goalsNorm[1]
  }

  A <- matrix(c(0, goalsNorm[2], goalsNorm[1], 0), 2, 2, TRUE)
  meanGoalsData <- list()
  meanGoalsData[["A"]] <- A
  meanGoalsData[["meanGoals"]] <- meanGoals
  meanGoalsData
}
