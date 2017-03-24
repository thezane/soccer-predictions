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
  
  # Update sse of expected and actual game outcome
  sse <- computeGameCost(game, gamePrediction)
  rOutput <- updateStrCost(rOutput, sse)
  game <- updateSSE(game, sse)
  
  # Update sse of expected and actual goals
  goalsExpected <- gamePrediction[["goalsExpected"]]
  goalsActual <- game$goals
  rOutput <- updateGoalsCost(rOutput, goalsExpected, goalsActual)
  
  # Outputing costs
  costData <- list(rOutput=rOutput, game=game)

  if (game$teamNames[1] == "New Zealand" || game$teamNames[2] == "New Zealand") {
    print(game$teamNames)
    print(gamePrediction$gamePs)
    print(gamePrediction$goalsExpected)
    print(goalsActual)
    print(computeHuberCost(computeSSE(goalsExpected, goalsActual)))
  }
  costData
}

# Compute sse of expected and actual game outcome
computeGameCost <- function(game, gamePrediction) {
  goals <- game$goals
  gamePs <- gamePrediction[["gamePs"]]
  expectedResult <- gamePs
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  sse <- computeSSE(actualResult, expectedResult)
  sse
}
