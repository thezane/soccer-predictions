forecastGame <- function (gameHypo=NULL, model=NULL, game=NULL) {
  maxGoals <- 20
  numDecimals <- 4
  isTraining <- !is.null(model)

  if (!isTraining) {
    rData <- gameHypo$rData
    model <- rData[["rOptions"]]$model
    game <- gameHypo
  }

  meanGoals <- game$meanGoals
  homeMeanGoals <- meanGoals[1]
  awayMeanGoals <- meanGoals[2]
  strNorm <- game$strNorm  
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  lambdas <- computeLambdas(model, homeMeanGoals, awayMeanGoals,
      homeStr, awayStr)
  theta <- model$theta
  inflatedP <- model$p
  gamePrediction <- computeGamePrediction(lambdas, theta, inflatedP,
      maxGoals)
  geomMean <- 1 / theta
  gamePrediction[["goalsExpected"]] <- c(
      fDIBP(lambdas[1] + lambdas[3], geomMean, inflatedP),
      fDIBP(lambdas[2] + lambdas[3], geomMean, inflatedP))

  if (!isTraining) {
    gamePrediction[["homeAwayGoals"]] <- round(
        gamePrediction[["homeAwayGoals"]], numDecimals)
    gamePrediction[["goalsExpected"]] <- round(
        gamePrediction[["goalsExpected"]], numDecimals)
  }

  gamePrediction
}

computeLambdas <- function(model, homeMeanGoals, awayMeanGoals,
    homeStr, awayStr) {
  aBeta <- model$aBeta
  dBeta <- model$dBeta
  lambda1Log <- log(homeMeanGoals) +
      dBeta * awayStr[2] +
      aBeta * homeStr[1]
  lambda2Log <- log(awayMeanGoals) +
      dBeta * homeStr[2] +
      aBeta * awayStr[1]
  lambda3Log <- model$corrBeta
  lambdas <- exp(c(lambda1Log, lambda2Log, lambda3Log))
  lambdas
}

computeGamePrediction <- function(lambdas, geomP, inflatedP,
    maxGoals) {
  n <- maxGoals + 1
  homeAwayGoals <- matrix(nrow=n, ncol=n)
  gamePs <- rep(0, 3)
  i <- 1
  
  while (i <= n) {
    j <- 1
    
    while (j <= n) {
      homeGoals <- i - 1
      awayGoals <- j - 1
      fBP <- pbivpois(homeGoals, awayGoals, lambdas)
      
      if (homeGoals == awayGoals) {
		fD <- dgeom(homeGoals, geomP)
        p <- fDIBP(fBP, fD, inflatedP)
      }
      else {
        p <- fDIBP(fBP, 0, inflatedP)
      }
      
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

fDIBP <- function(fBP, fD, p) {
  (1 - p) * fBP + p * fD
}
