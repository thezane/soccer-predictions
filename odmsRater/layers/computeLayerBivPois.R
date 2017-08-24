# Compute probability of each scoreline with model parameters rOptions.
computeLayerBivPois <- function(game, rOptions) {
  meanGoals <- game$meanGoals
  homeMeanGoals <- meanGoals[1]
  awayMeanGoals <- meanGoals[2]
  strNorm <- game$strNorm 
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  lambdas <- computeLambdas(rOptions, homeMeanGoals, awayMeanGoals,
      homeStr, awayStr)
  pGoals <- computePGoals(lambdas, rOptions)
  goalsExpected <- c(lambdas[1], lambdas[2]) + lambdas[3]
  gamePrediction <- list(goalsExpected=goalsExpected, pGoals=pGoals)
  gamePrediction
}

computeLambdas <- function(rOptions, homeMeanGoals, awayMeanGoals,
    homeStr, awayStr) {
  lambda1Log <- log(homeMeanGoals) +
      rOptions$strBeta * (awayStr[2] + homeStr[1])
  lambda2Log <- log(awayMeanGoals) +
      rOptions$strBeta * (homeStr[2] + awayStr[1])
  lambda3Log <- rOptions$corrBeta
  lambdas <- exp(c(lambda1Log, lambda2Log, lambda3Log))
  lambdas
}

computePGoals <- function(lambdas, rOptions) {
  n <- rOptions$pGoalsMatSize
  pGoals <- matrix(0, nrow=n, ncol=n)
  i <- 1
  
  while (i <= n) {
    j <- 1
    
    while (j <= n) {
      homeGoals <- i - 1
      awayGoals <- j - 1
      pGoals[i, j] <- pbivpois(homeGoals, awayGoals, lambdas)
      j <- j + 1
    }
    
    i <- i + 1
  }
  
  pGoals
}
