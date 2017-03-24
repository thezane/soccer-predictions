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
  expectedResult <- gamePrediction[["gamePs"]]
  actualResult <- computeGameCost(game, gamePrediction)
  rOutput <- updateStrCost(rOutput, expectedResult, actualResult)
  game <- updateSSE(game, computeSSE(expectedResult, actualResult))
  
  # Update cost of expected and actual goals
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
    print(computeHuberCost(goalsExpected - goalsActual))
  }
  costData
}

# Compute actual game outcome
computeGameOutcome <- function(game) {
  goals <- game$goals
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  actualResult
}
