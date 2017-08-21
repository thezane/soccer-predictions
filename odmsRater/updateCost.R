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
  gamePrediction <- computeLayerPois(game, rOptions)
  
  # Update cost of outcome
  resultExpected <- gamePrediction[["gamePs"]]
  resultActual <- game$outcome
  game <- computeSSE.Game(game, resultExpected, resultActual)
  game$Ps <- gamePrediction[["gamePs"]]
  
  # Update cost of goals
  goals <- game$goals
  homeAwayGoals <- gamePrediction[["homeAwayGoals"]]
  p <- homeAwayGoals[goals[1] + 1, goals[2] + 1]
  weight <- min(game$reliability) * game$weight
  rOutput <- updateGoalsCost.RatingsOutput(rOutput, p, weight,
      game$dataset)
  costData <- list(rOutput=rOutput, game=game)
  costData
}
