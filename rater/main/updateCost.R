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
  gamePrediction <- forecastGame(model=rOptions$model, game=game)
  strCostData <- computeStrCost(game, gamePrediction)
  sse <- strCostData[["sse"]]
  rOutput <- updateStrCosts(rOutput, strCostData[["goalsExpected"]],
      sse)
  game <- updateSSE(game, sse)
  costData <- list(rOutput=rOutput, game=game)
  costData
}

computeStrCost <- function(game, gamePrediction) {
  goals <- game$goals
  gamePs <- gamePrediction[["gamePs"]]
  expectedResult <- gamePs
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  sse <- sum(computeSSE(actualResult, expectedResult))
  goals <- game$goals
  goalsExpected <- sum(gamePrediction[["goalsExpected"]])
  strCostData <- list(goalsExpected=goalsExpected, sse=sse)
  strCostData
}
