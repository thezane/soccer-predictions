forecastMatch <- function (homeTeam, awayTeam, contest,
    forecastPrereq, locations=c()) {
  maxGoals <- 20
  numDecimals <- 4
  meanGoalsMap <- forecastPrereq[["MeanGoalsMap"]]
  matches <- forecastPrereq[["Matches"]]
  model <- forecastPrereq[["Model"]]
  teams <- forecastPrereq[["Teams"]]
  homeContest = paste(contest, "Home", sep="")
  awayContest = paste(contest, "Away", sep="")
  
  if (contest == "q" || is.element(homeTeam, locations)) {
    homeMeanGoals <- meanGoalsMap[[homeContest]]
    awayMeanGoals <- meanGoalsMap[[awayContest]]
  }
  else {
    homeMeanGoals <- meanGoalsMap[[awayContest]]
    awayMeanGoals <- homeMeanGoals
  }
  
  homeStr <- teams[[homeTeam]]
  awayStr <- teams[[awayTeam]]
  lambdas <- computeLambdas(model, homeStr, awayStr, homeMeanGoals,
      awayMeanGoals)
  geomP <- model[["theta"]]
  inflatedP <- model[["p"]]
  geomMean <- 1 / geomP
  matchPrediction <- computeMatchPrediction(lambdas, geomP, inflatedP,
      maxGoals, numDecimals)
  matchPrediction["HomeGoals"] <- round(
      inflateY(lambdas[1] + lambdas[3], geomMean, inflatedP),
      numDecimals)
  matchPrediction["AwayGoals"] <- round(
      inflateY(lambdas[2] + lambdas[3], geomMean, inflatedP),
      numDecimals)
  matchPrediction
}

computeLambdas <- function(model, homeStr, awayStr,
    homeMeanGoals, awayMeanGoals) {
  lambda1 <- model$beta1[[1]] +
      model$beta1[[2]] * awayStr[[2]] +
      model$beta1[[3]] * homeStr[[1]] +
      model$beta1[[4]] * homeMeanGoals
  lambda2 <- model$beta2[[1]] +
      model$beta2[[2]] * homeStr[[2]] +
      model$beta2[[3]] * awayStr[[1]] +
      model$beta2[[4]] * awayMeanGoals
  lambda3 <- model$beta3[[1]]
  lambdas <- exp(c(lambda1, lambda2, lambda3))
  lambdas
}

computeMatchPrediction <- function(lambdas, geomP, inflatedP,
    maxGoals, numDecimals) {
  n <- maxGoals + 1
  homeAwayGoals <- matrix(nrow=n, ncol=n)
  matchPs <- list("HomeWin"=0, "Tie"=0, "AwayWin"=0)
  i <- 1
  
  while (i <= n) {
    j <- 1
    
    while (j <= n) {
      homeGoals <- i - 1
      awayGoals <- j - 1
      fBP <- pbivpois(homeGoals, awayGoals, lambdas)
      
      if (homeGoals == awayGoals) {
        p <- inflateY(fBP, dgeom(homeGoals, geomP), inflatedP)
      }
      else {
        p <- inflateY(fBP, 0, inflatedP)
      }
      
      p <- round(p, numDecimals)
      homeAwayGoals[i, j] <- p
      matchPs <- updateMatchPs(matchPs, homeGoals, awayGoals, p)      
      j <- j + 1
    }
    
    i <- i + 1
  }
  
  list("HomeAwayGoals"=homeAwayGoals, "MatchPs"=matchPs)
}

updateMatchPs <- function(matchPs, homeGoals, awayGoals, p) {
  if (homeGoals < awayGoals) {
    matchPs["AwayWin"] <- matchPs[["AwayWin"]] + p
  }
  else if (homeGoals == awayGoals) {
    matchPs["Tie"] <- matchPs[["Tie"]] + p
  }
  else {
    matchPs["HomeWin"] <- matchPs[["HomeWin"]] + p
  }
  
  matchPs
}

inflateY <- function(x, y, p) {
  (1 - p) * x + p * y
}
