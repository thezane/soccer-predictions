updateCost <- function(rOptions, rOutput, game, gamePrev) {

  if (!is.null(gamePrev) && gamePrev$year < game$year) {
    rOutput <- updateStrMeanCosts.RatingsOutput(rOutput, game$dataset)
  }

  if (game$isRelevant) {
    costData <- updateRatingsCost(rOptions, rOutput, game)
  }
  else {
    costData <- list(rOutput=rOutput, game=game)
  }

  costData
}

updateRatingsCost <- function(rOptions, rOutput, game) {
  gamePredictionBivPois <- computeLayerBivPois(game, rOptions)
  gamePredictionPois <- computeLayerPois(game, rOptions)
  gamePrediction <- computeLayerMixture(game,
      gamePredictionBivPois, gamePredictionPois, rOptions)
  
  # Update cost of outcome
  resultExpected <- gamePrediction[["pWinTieLoss"]]
  resultActual <- game$outcome
  game <- computeSSE.Game(game, resultExpected, resultActual)
  game$Ps <- resultExpected
  
  # Update cost of goals
  goals <- game$goals
  homeAwayGoals <- gamePrediction[["pGoals"]]
  p <- homeAwayGoals[goals[1] + 1, goals[2] + 1]
  weight <- min(game$reliability) * game$weight
  rOutput <- updateGoalsCost.RatingsOutput(rOutput, p, weight,
      game$dataset)
  costData <- list(rOutput=rOutput, game=game)
  costData
}
