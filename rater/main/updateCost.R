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
  mse <- strCostData[["mse"]]
  rOutput <- updateStrCost(rOutput, computeTukeyCost(mse),
      strCostData[["goalsExpected"]], game$teamXP)
  game <- updateMSE(game, mse)
  costData <- list(rOutput=rOutput, game=game)
  costData
}

computeStrCost <- function(game, gamePrediction) {
  goals <- game$goals
  gamePs <- gamePrediction[["gamePs"]]
  expectedResult <- gamePs
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  mse <- computeMSE(actualResult, expectedResult)
  goalsExpected <- sum(gamePrediction[["goalsExpected"]])
  strCostData <- list(goalsExpected=goalsExpected, mse=mse)
  strCostData
}

computeHuberCost <- function(x, c=1.345) {
  xAbs <- abs(x)

  if (xAbs <= c) {
    huberCost <- x ^ 2
  }
  else {
    huberCost <- c * (2 * xAbs - c)
  }

  huberCost
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
