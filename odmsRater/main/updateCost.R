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
  gamePrediction <- forecastGame(game, rOptions)
  
  # Update cost of expected and actual game outcome
  goals <- game$goals
  homeAwayGoals <- gamePrediction[["homeAwayGoals"]]
  p <- homeAwayGoals[goals[1] + 1, goals[2] + 1]
  rOutput <- updateStrCost(rOutput, p)
  resultExpected <- gamePrediction[["gamePs"]]
  resultActual <- game$outcome
  game$Ps <- gamePrediction[["gamePs"]]
  game$sse <- computeSSE(resultExpected, resultActual)
  costData <- list(rOutput=rOutput, game=game)
  costData
}
