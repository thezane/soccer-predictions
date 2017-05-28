updateCost <- function(rOptions, rOutput, game, gamePrev) {

  if (!is.null(gamePrev) && game$isWocG && !gamePrev$isWocG) {
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
  rOutput <- updateOutcomeCost(rOutput, resultExpected, resultActual)
  game <- computeSSE(game, resultExpected, resultActual)
  game$Ps <- gamePrediction[["gamePs"]]
  
  # Update cost of goals
  goals <- game$goals
  homeAwayGoals <- gamePrediction[["homeAwayGoals"]]
  p <- homeAwayGoals[goals[1] + 1, goals[2] + 1]
  rOutput <- updateGoalsCost(rOutput, p)
  costData <- list(rOutput=rOutput, game=game)
  costData
}
