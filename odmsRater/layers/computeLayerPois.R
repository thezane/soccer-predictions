# Compute probability of each tied scoreline with model parameters
# rOptions.
computeLayerPois <- function (game, rOptions) {
  theta <- rOptions$theta
  n <- rOptions$pGoalsMatSize
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
