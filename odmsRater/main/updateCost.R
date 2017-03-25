updateCost <- function(rOptions, rOutput, game, gamePrev) {

  if (!is.null(gamePrev) && gamePrev$isQualifier && game$isWocG) {
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
  gamePrediction <- forecastGame(rOptions=rOptions, game=game)
  
  # Update cost of expected and actual game outcome
  resultExpected <- gamePrediction[["gamePs"]]
  resultActual <- game$outcome
  rOutput <- updateStrCost(rOutput, resultExpected, resultActual)
  game <- updateSSE(game, computeSSE(resultExpected, resultActual))
  
  # Update cost of expected and actual goals
  goalsExpected <- gamePrediction[["goalsExpected"]]
  goalsActual <- game$goals
  rOutput <- updateGoalsCost(rOutput, goalsExpected, goalsActual)
  costData <- list(rOutput=rOutput, game=game)
  costData
}
