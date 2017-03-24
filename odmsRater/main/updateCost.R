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
  resultActual <- computeGameCost(game, gamePrediction)
  rOutput <- updateStrCost(rOutput, resultExpected, resultActual)
  game <- updateSSE(game, computeSSE(resultExpected, resultActual))
  
  # Update cost of expected and actual goals
  goalsExpected <- gamePrediction[["goalsExpected"]]
  goalsActual <- game$goals
  rOutput <- updateGoalsCost(rOutput, goalsExpected, goalsActual)
  
  # Outputing costs
  rOutput <- updateOceania(rOutput, rOutput$tTree[[game$teamNames[1]]]$fName == "Asia" || rOutput$tTree[[game$teamNames[2]]]$fName == "Asia", resultExpected, resultActual, goalsExpected, goalsActual)

  if (rOutput$tTree[[game$teamNames[1]]]$fName == "Asia" || rOutput$tTree[[game$teamNames[2]]]$fName == "Asia") {
    print(game$teamNames)
    print(gamePrediction$gamePs)
    print(gamePrediction$goalsExpected)
    print(goalsActual)
  }
  
  costData <- list(rOutput=rOutput, game=game)
  costData
}

# Compute actual game outcome
computeGameOutcome <- function(game) {
  goals <- game$goals
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  actualResult
}
