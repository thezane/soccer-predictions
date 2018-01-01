computeLayers.RatingsOptions <- function(rOptions, game) {
  gamePrediction <- NULL
  strNextNorm <- NULL
  meanGoals <- computeLayerHa(game, rOptions)

  if (game$isRelevant || rOptions$isOptimized) {
    gamePredictionBivPois <- computeLayerBivPois(game, rOptions,
        meanGoals)
    gamePredictionPois <- computeLayerPois(game, gamePredictionBivPois,
        rOptions)
    gamePrediction <- computeLayerMixture(game,
        gamePredictionBivPois, gamePredictionPois, rOptions)
  }

  if (game$hasOutcome) {
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoals)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)
  }

  layerOutput <- list(gamePrediction=gamePrediction,
      strNextNorm=strNextNorm)
  layerOutput
}
