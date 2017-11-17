# Compute probability of each tied scoreline with model parameters
# rOptions.
computeLayerPois <- function (game, gamePredictionBivPois, rOptions) {
  theta <- rOptions$theta
  pGoals <- gamePredictionBivPois[["pGoals"]]
  n <- nrow(pGoals)
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
