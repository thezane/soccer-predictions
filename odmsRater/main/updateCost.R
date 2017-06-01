updateCost <- function(rOptions, rOutput, game, gamePrev) {

  if (!is.null(gamePrev) && gamePrev$year < game$year) {
    rOutput <- updateStrMeanCosts(rOutput)
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
  game <- computeSSE(game, resultExpected, resultActual)
  game$Ps <- gamePrediction[["gamePs"]]
  
  # Update cost of goals
  goals <- game$goals
  homeAwayGoals <- gamePrediction[["homeAwayGoals"]]
  p <- homeAwayGoals[goals[1] + 1, goals[2] + 1]
  rOutput <- updateGoalsCost(rOutput, p, game$weight)
  costData <- list(rOutput=rOutput, game=game)
  costData
}
