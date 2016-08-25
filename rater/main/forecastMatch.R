forecastMatch <- function (game, model) {
  maxGoals <- 20
  numDecimals <- 4
  meanGoals <- game$meanGoals
  homeMeanGoals <- meanGoals[1]
  awayMeanGoals <- meanGoals[2]
  strNorm <- game$strNorm  
  homeStr <- strNorm[1, ]
  awayStr <- strNorm[2, ]
  lambdas <- computeLambdas(model, homeMeanGoals, awayMeanGoals,
      homeStr, awayStr)
  matchPs <- computeMatchPrediction(lambdas, model$theta, model$p,
      maxGoals)
  matchPs
}

computeLambdas <- function(model, homeMeanGoals, awayMeanGoals,
    homeStr, awayStr) {
  aBeta <- model$aBeta
  dBeta <- model$dBeta
  lambda1Log <- log(homeMeanGoals) +
      aBeta * awayStr[2] +
      dBeta * homeStr[1]
  lambda2Log <- log(awayMeanGoals) +
      aBeta * homeStr[2] +
      dBeta * awayStr[1]
  lambda3Log <- model$corrBeta
  lambdas <- exp(c(lambda1Log, lambda2Log, lambda3Log))
  lambdas
}

computeMatchPrediction <- function(lambdas, geomP, inflatedP,
    maxGoals) {
  n <- maxGoals + 1
  homeAwayGoals <- matrix(nrow=n, ncol=n)
  matchPs <- rep(0, 3)
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
      matchPs <- updateMatchPs(matchPs, homeGoals, awayGoals, p)      
      j <- j + 1
    }
    
    i <- i + 1
  }
  
  matchPs
}

updateMatchPs <- function(matchPs, homeGoals, awayGoals, p) {
  if (homeGoals > awayGoals) {
    matchPs[1] <- matchPs[1] + p
  }
  else if (homeGoals == awayGoals) {
    matchPs[2] <- matchPs[2] + p
  }
  else {
    matchPs[3] <- matchPs[3] + p
  }
  
  matchPs
}

fDIBP <- function(fBP, fD, p) {
  (1 - p) * fBP + p * fD
}
