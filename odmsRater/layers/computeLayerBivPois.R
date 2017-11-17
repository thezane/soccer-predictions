# Compute probability of each scoreline with bivariate poisson model
# and model parameters rOptions.
computeLayerBivPois <- function(game, rOptions, meanGoals) {
  homeMeanGoals <- meanGoals[1]
  awayMeanGoals <- meanGoals[2]
  strNorm <- game$strNorm 
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  lambdas <- computeLambdas(rOptions, homeMeanGoals, awayMeanGoals,
      homeStr, awayStr)
  gamePrediction <- computePredictionBivPois(lambdas, game, rOptions)
  goalsOutput <- game$goalsOutput
  pGoals <- gamePrediction[["pGoals"]]
  p <- pGoals[goalsOutput[1] + 1, goalsOutput[2] + 1]
  gamePrediction[["p"]] <- p
  gamePrediction[["strNormBeta"]] <- game$strNormBeta
  gamePrediction[["strAgg"]] <- game$strAgg
  gamePrediction
}

computeLambdas <- function(rOptions, homeMeanGoals, awayMeanGoals,
    homeStr, awayStr) {
  lambda1Log <- log(homeMeanGoals) +
      rOptions$strBeta * (homeStr[1] + awayStr[2])
  lambda2Log <- log(awayMeanGoals) +
      rOptions$strBeta * (awayStr[1] + homeStr[2])
  lambda3Log <- rOptions$corrBeta
  lambdas <- exp(c(lambda1Log, lambda2Log, lambda3Log))
  lambdas
}

computePredictionBivPois <- function(lambdas, game, rOptions) {
  goalsExpected <- c(lambdas[1], lambdas[2]) + lambdas[3]
  goalsMax <- ceiling(max(c(game$goals, goalsExpected)))
  n <- rOptions$pGoalsMatSize + goalsMax
  pGoals <- matrix(0, nrow=n, ncol=n)
  pWinTieLose <- c(0, 0, 0)
  i <- 1
  
  while (i <= n) {
    j <- 1
    
    while (j <= n) {
      homeGoals <- i - 1
      awayGoals <- j - 1
      p <- pbivpois(homeGoals, awayGoals, lambdas)
      pGoals[i, j] <- p
      pWinTieLose <- pWinTieLose + p * as.numeric(
          c(i > j, i == j, i < j))
      j <- j + 1
    }
    
    i <- i + 1
  }
  
  gamePrediction <- list(goalsExpected=goalsExpected, pGoals=pGoals,
      pWinTieLose=pWinTieLose)
  gamePrediction
}
