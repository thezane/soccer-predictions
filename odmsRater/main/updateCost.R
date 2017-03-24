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
  resultActual <- computeGameOutcome(game)
  rOutput <- updateStrCost(rOutput, resultExpected, resultActual)
  game <- updateSSE(game, computeSSE(resultExpected, resultActual))
  
  # Update cost of expected and actual goals
  goalsExpected <- gamePrediction[["goalsExpected"]]
  goalsActual <- game$goals
  rOutput <- updateGoalsCost(rOutput, goalsExpected, goalsActual)
  
  # Outputing costs
  rOutput <- updateOceania(rOutput, rOutput$tTree[[game$teamNames[1]]]$fName == "Oceania" || rOutput$tTree[[game$teamNames[2]]]$fName == "Oceania", resultExpected, resultActual, goalsExpected, goalsActual)

  if (rOutput$tTree[[game$teamNames[1]]]$fName == "Oceania" || rOutput$tTree[[game$teamNames[2]]]$fName == "Oceania") {
	print(game$teamNames)
	print(goalsExpected)
	print(goalsActual)
    print(resultExpected)
    print(resultActual)
    print(computeSSE(resultExpected, resultActual))
  }
  
  costData <- list(rOutput=rOutput, game=game)
  costData
}

# Compute actual game outcome
computeGameOutcome <- function(game) {
  goals <- game$goals
  resultActual <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  resultActual
}
