# Compute probability of each goal difference with skellam pdf and
# model parameters rOptions.
computeLayerSkellam <- function(game, rOptions, meanGoals) {
  homeMeanGoals <- meanGoals[1]
  awayMeanGoals <- meanGoals[2]
  strNorm <- game$strNorm 
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  lambdas <- computeLambdasSkellam(rOptions,
      homeMeanGoals, awayMeanGoals, homeStr, awayStr)
  gamePrediction <- computePredictionSkellam(lambdas, game, rOptions)
  gamePrediction[["strNormBeta"]] <- game$strNormBeta
  gamePrediction[["strAgg"]] <- game$strAgg
  gamePrediction
}

computeLambdasSkellam <- function(rOptions,
    homeMeanGoals, awayMeanGoals, homeStr, awayStr) {
  lambda1Log <- log(homeMeanGoals) +
      rOptions$strBeta * (homeStr[1] + awayStr[2])
  lambda2Log <- log(awayMeanGoals) +
      rOptions$strBeta * (awayStr[1] + homeStr[2])
  lambdas <- exp(c(lambda1Log, lambda2Log))
  lambdas
}

computePredictionSkellam <- function(lambdas, game, rOptions) {
  goalsDiffExpected <- lambdas[1] - lambdas[2]
  goalsDiff <- game$goals[1] - game$goals[2]
  goalsDiffMax <- ceiling(max(abs(c(goalsDiffExpected, goalsDiff))))
  n <- rOptions$pGoalsMatSize + goalsDiffMax

  # Goal difference probabilities between home and away teams
  pGoalsDiff <- hash()
  pWinTieLose <- c(0, 0, 0)
  i <- -n
  
  while (i <= n) {
    p <- dskellam(i, lambdas[1], lambdas[2])
    pGoalsDiff[i] <- p
    pWinTieLose <- pWinTieLose + p * as.numeric(
          c(i > 0, i == 0, i < 0))
    i <- i + 1
  }
  
  pOutcome <- pGoalsDiff[[as.character(goalsDiff)]]
  gamePrediction <- list(goalsDiffExpected=goalsDiffExpected,
      p=pOutcome, pGoalsDiff=pGoalsDiff, pWinTieLose=pWinTieLose)
  gamePrediction
}
