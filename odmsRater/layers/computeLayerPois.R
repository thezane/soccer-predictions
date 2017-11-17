# Compute probability of each tied scoreline with model parameters
# rOptions.
computeLayerPois <- function (game, gamePredictionBivPois, rOptions) {
  pGoals <- gamePredictionBivPois[["pGoals"]]
  theta <- rOptions$theta + nrow(pGoals)
  n <- game$pGoalsMatSize
  pGoals <- matrix(0, nrow=n, ncol=n)
  i <- 1
  
  while (i <= n) {
    homeGoals <- i - 1
    pGoals[i, i] <- dpois(homeGoals, theta)
    i <- i + 1
  }
  
  gamePrediction <- list(goalsExpected=theta, pGoals=pGoals)
  gamePrediction
}
