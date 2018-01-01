# Compute probability of each scoreline with probabilities from
# bivariate and univariate poisson pdfs and model parameters rOptions.
computeLayerMixture <- function (game, gamePredictionBivPois,
    gamePredictionPois, rOptions) {
  strNorm <- game$strNorm 
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  p <- computePoisWeight(rOptions$tieBias, rOptions$tieBeta,
      game$strAgg)
  gamePrediction <- computePrediction(gamePredictionBivPois,
      gamePredictionPois, p, game)
  gamePrediction
}

computePoisWeight <- function(tieBias, tieBeta, strAgg) {
  plogis(tieBias + tieBeta * diff(strAgg) ^ 2)
}

computePrediction <- function(gamePredictionBivPois,
    gamePredictionPois, p, game) {
  goalsExpectedBivPois <- gamePredictionBivPois[["goalsExpected"]]
  goalsExpectedPois <- gamePredictionPois[["goalsExpected"]]
  pGoalsBivPois <- gamePredictionBivPois[["pGoals"]]
  pGoalsPois <- gamePredictionPois[["pGoals"]]
  goalsExpected <- (1 - p) * goalsExpectedBivPois +
      p * goalsExpectedPois
  pGoals <- (1 - p) * pGoalsBivPois + p * pGoalsPois
  pWinTieLose <- c(
      sum(pGoals[lower.tri(pGoals)]),
      sum(diag(pGoals)),
      sum(pGoals[upper.tri(pGoals)]))
  gamePrediction <- list()
  gamePrediction[["goalsExpected"]] <- goalsExpected
  gamePrediction[["pGoals"]] <- pGoals
  gamePrediction[["pWinTieLose"]] <- pWinTieLose
  gamePrediction[["strNormBeta"]] <-
      gamePredictionBivPois[["strNormBeta"]]
  gamePrediction[["strAgg"]] <- gamePredictionBivPois[["strAgg"]]
  
  if (game$hasOutcome) {
	pOutcome <- pGoals[game$goalsOutcome[1] + 1, game$goalsOutcome[2] + 1]
    gamePrediction[["p"]] <- pOutcome
  }
  
  gamePrediction
}
