computeLayerHa <- function(game, rOptions) {
  meanGoals <- rOptions$meanGoals * c(1, 1)

  if (game$existsHa) {
    homeMeanGoals <- rOptions$meanGoals + rOptions$haBias
    meanGoals[1] <- homeMeanGoals
  }

  meanGoals
}
