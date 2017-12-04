computeLayers.RatingsOptionsSkellamNonMov <- function(rOptions, game) {
  gamePrediction <- NULL
  strNextNorm <- NULL
  meanGoals <- computeLayerHa(game, rOptions)

  if (game$isRelevant || rOptions$isOptimized) {
    gamePrediction <- computeLayerSkellam(game, rOptions, meanGoals)
  }

  if (game$computeRatings) {
	game <- computeLayerGoalsNonMov(game)
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoals)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)
  }

  layerOutput <- list(gamePrediction=gamePrediction,
      strNextNorm=strNextNorm)
  layerOutput
}
