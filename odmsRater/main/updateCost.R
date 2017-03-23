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
  
  # Updating sse of expected result and actual result
  sse <- computeGameCost(game, gamePrediction)
  rOutput <- updateStrCost(rOutput, computeTukeyCost(sse))
  game <- updateSSE(game, sse)
  
  # Updating sse of expected goals and actual goals
  goalsExpected <- gamePrediction[["goalsExpected"]]
  goalsActual <- game$goals
  rOutput <- updateGoalsCost(rOutput, goalsExpected, goalsActual)
  
  # Outputing costs
  costData <- list(rOutput=rOutput, game=game)
  costData
}

computeGameCost <- function(game, gamePrediction) {
  goals <- game$goals
  gamePs <- gamePrediction[["gamePs"]]
  expectedResult <- gamePs
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  sse <- computeSSE(actualResult, expectedResult)
  sse
}

# Apply the biweight function to 'x'
computeTukeyCost <- function(x, c=4.685) {
  xAbs <- abs(x)

  if (xAbs <= c) {
    tukeyCost <- x ^ 6 / (6 * c ^ 4) - (x ^ 4) / (2 * c ^ 2) +
        x ^ 2 / 2
  }
  else {
    tukeyCost <- c ^ 2 / 6
  }

  tukeyCost
}
