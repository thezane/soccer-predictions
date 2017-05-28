computeLayerPois <- function (game, rOptions) {
  maxGoals <- 40
  meanGoals <- game$meanGoals
  homeMeanGoals <- meanGoals[1]
  awayMeanGoals <- meanGoals[2]
  strNorm <- game$strNorm  
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  lambdas <- computeLambdas(rOptions, homeMeanGoals, awayMeanGoals,
      homeStr, awayStr)
  gamePrediction <- computeGamePrediction(lambdas, maxGoals)
  gamePrediction[["goalsExpected"]] <- c(lambdas[1], lambdas[2]) +
      lambdas[3]
  gamePrediction[["strNorm"]] <- strNorm
  gamePrediction[["strAgg"]] <- game$strAgg
  gamePrediction
}

computeLambdas <- function(rOptions, homeMeanGoals, awayMeanGoals,
    homeStr, awayStr) {
  strBeta <- rOptions$strBeta
  lambda1Log <- log(homeMeanGoals) +
      strBeta * awayStr[2] +
      strBeta * homeStr[1]
  lambda2Log <- log(awayMeanGoals) +
      strBeta * homeStr[2] +
      strBeta * awayStr[1]
  lambda3Log <- rOptions$corrBeta
  lambdas <- exp(c(lambda1Log, lambda2Log, lambda3Log))
  lambdas
}

computeGamePrediction <- function(lambdas, maxGoals) {
  n <- maxGoals + 1
  homeAwayGoals <- matrix(nrow=n, ncol=n)
  gamePs <- rep(0, 3)
  i <- 1
  
  while (i <= n) {
    j <- 1
    
    while (j <= n) {
      homeGoals <- i - 1
      awayGoals <- j - 1
      p <- pbivpois(homeGoals, awayGoals, lambdas)
      homeAwayGoals[i, j] <- p
      gamePs <- updateGamePs(gamePs, homeGoals, awayGoals, p)
      j <- j + 1
    }
    
    i <- i + 1
  }
  
  gamePrediction <- list(homeAwayGoals=homeAwayGoals, gamePs=gamePs)
  gamePrediction
}

updateGamePs <- function(gamePs, homeGoals, awayGoals, p) {
  if (homeGoals > awayGoals) {
    gamePs[1] <- gamePs[1] + p
  }
  else if (homeGoals == awayGoals) {
    gamePs[2] <- gamePs[2] + p
  }
  else {
    gamePs[3] <- gamePs[3] + p
  }
  
  gamePs
}
