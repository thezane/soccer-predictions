computeLayers.RatingsOptionsSoftmax <- function(rOptions, game) {
  gamePrediction <- NULL
  strNextNorm <- NULL
  meanGoals <- computeLayerHa(game, rOptions)

  if (game$isRelevant || rOptions$isOptimized) {
    gamePrediction <- computeLayerSoftmax(game, rOptions, meanGoals)
  }

  if (game$hasOutcome) {
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoals)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)
  }

  layerOutput <- list(gamePrediction=gamePrediction,
      strNextNorm=strNextNorm)
  layerOutput
}
