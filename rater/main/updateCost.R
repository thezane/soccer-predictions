updateCost <- function(rOptions, rOutput, game, gamePrev) {

  if (!is.null(gamePrev) && gamePrev$isQualifier &&
        game$isWorldCupGroup) {
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
  matchPs <- forecastMatch(game, rOptions$model)
  t <- as.numeric(rOutput$currentDate - game$gameDate)
  strCost <- computeStrCost(game, matchPs)
  rOutput <- updateStrCost(rOutput,
      expDecay(t, rOutput$kCost, strCost))
  game <- updateMSE(game, strCost)
  costData <- list(rOutput=rOutput, game=game)
  costData
}

computeStrCost <- function(game, matchPs) {
  goals <- game$goals
  expectedResult <- matchPs
  actualResult <- as.numeric(c(goals[1] > goals[2],
      goals[1] == goals[2], goals[1] < goals[2]))
  mse <- computeMSE(actualResult, expectedResult)
  mse
}
