forecastMatch <- function (homeTeam, awayTeam, contest, teams.=teams,
    model.=model, meanGoalsMap.=meanGoalsMap) {
  MAX_GOALS <- 10
  NUM_DECIMALS <- 4
  
  if (contest == "qualifier") {
    homeMeanGoals <- meanGoalsMap[[paste(contest, "Home", sep="")]]
    awayMeanGoals <- meanGoalsMap[[paste(contest, "Away", sep="")]]
  }
  else {
    homeMeanGoals <- meanGoalsMap[[contest]]
    awayMeanGoals <- homeMeanGoals
  }
  
  homeStr <- teams[[homeTeam]]
  awayStr <- teams[[awayTeam]]
  lambdas <- computeLambdas(model, homeStr, awayStr, homeMeanGoals,
      awayMeanGoals)
  matchResults <- computeMatchResults(lambdas, MAX_GOALS, NUM_DECIMALS)
  matchResults["HomeGoals"] <- round(lambdas[1], NUM_DECIMALS)
  matchResults["AwayGoals"] <- round(lambdas[2], NUM_DECIMALS)
  matchResults
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

computeMatchResults <- function(lambdas, maxGoals, numDecimals) {
  n <- maxGoals + 1
  homeAwayGoals <- matrix(nrow=n, ncol=n)
  matchProbs <- list("HomeWin"=0, "AwayWin"=0, "Tie"=0)
  i <- 1
  
  while (i <= n) {
    j <- 1
    
    while (j <= n) {
      homeGoals <- i - 1
      awayGoals <- j - 1
      p <- round(pbivpois(homeGoals, awayGoals, lambdas), numDecimals)
      homeAwayGoals[i, j] <- p
      matchProbs <- updateMatchProbs(matchProbs, homeGoals, awayGoals,
          p)      
      j <- j + 1
    }
    
    i <- i + 1
  }
  
  list("HomeAwayGoals"=homeAwayGoals, "MatchProbs"=matchProbs)
}

updateMatchProbs <- function(matchProbs, homeGoals, awayGoals, p) {
  if (homeGoals < awayGoals) {
    matchProbs["AwayWin"] <- matchProbs[["AwayWin"]] + p
  }
  else if (homeGoals == awayGoals) {
    matchProbs["Tie"] <- matchProbs[["Tie"]] + p
  }
  else {
    matchProbs["HomeWin"] <- matchProbs[["HomeWin"]] + p
  }
  
  matchProbs
}
