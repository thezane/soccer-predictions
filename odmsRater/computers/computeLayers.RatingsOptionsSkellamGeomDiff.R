computeLayers.RatingsOptionsSkellamGeomDiff <- function(rOptions, 
    game) {
  gamePrediction <- NULL
  strNextNorm <- NULL
  meanGoals <- computeLayerHa(game, rOptions)

  if (game$isRelevant || rOptions$isOptimized) {
    gamePrediction <- computeLayerSkellam(game, rOptions, meanGoals)
  }

  if (game$hasOutcome) {
	game <- computeLayerGoalsGeomDiff(game, rOptions)
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoals)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)
  }

  layerOutput <- list(gamePrediction=gamePrediction,
      strNextNorm=strNextNorm)
  layerOutput
}
