# Compute probability of each scoreline with probabilities from
# bivariate and univariate poisson models and model parameters
# rOptions.
computeLayerMixture <- function (game, gamePredictionBivPois,
    gamePredictionPois, rOptions) {
  strNorm <- game$strNorm 
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  p <- computeMixtureWeight(rOptions$tieBias, rOptions$tieBeta,
      game$strAgg)
  gamePrediction <- computePrediction(p, gamePredictionBivPois,
      gamePredictionPois)
  goals <- game$goals
  pGoals <- gamePrediction[["pGoals"]]
  p <- pGoals[goals[1] + 1, goals[2] + 1]
  gamePrediction[["p"]] <- p
  gamePrediction
}

computeMixtureWeight <- function(tieBias, tieBeta, strAgg) {
  plogis(tieBias + tieBeta * abs(diff(strAgg)))
}

computePrediction <- function(p, gamePredictionBivPois,
    gamePredictionPois) {
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
  gamePrediction
}
