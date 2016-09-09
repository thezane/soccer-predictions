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
  rOutput <- updateStrCost(rOutput, computeTukeyCost(sse),
      strCostData[["goalsExpected"]], game$teamXP)
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
  sse <- computeSSE(actualResult, expectedResult)
  goalsExpected <- sum(gamePrediction[["goalsExpected"]])
  strCostData <- list(goalsExpected=goalsExpected, sse=sse)
  strCostData
}

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

computeSSE <- function(x1, x2) {
  sum((x1 - x2) ^ 2)
} 
