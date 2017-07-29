computeRNN <- function(rOptions, rOutput) {
  rOutput <- resetRatingsOutput(rOutput)
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  tTree <- iterTeams(rOptions, tTree, resetTeam)
  gi <- reset(gi)
  gamePrev <- NULL
  i <- 1
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    game <- normalizeGoals(game, rOptions)
    strPrereqs <- constructStrPrereqs(rOptions, game, gamePrev, tTree)
    updateStrData <- updateStr(strPrereqs, rOptions)
    tTree <- updateStrData[["tTree"]]
    game <- updateStrData[["game"]]
    costData <- updateCost(rOptions, rOutput, game, gamePrev)
    rOutput <- costData[["rOutput"]]
    game <- costData[["game"]]
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- game
    gTree[game$gameDateStr] <- gDateList
    gamePrev <- game
    i <- i + 1
  }
  
  rOutput <- updateTotalCosts(rOutput, rOptions)
  rOutput$tTree <- tTree
  rOutput$gTree <- gTree
  rOutput$gi <- gi
  rOutput
}

iterTeams <- function(rOptions, tTree, f) {
  teams <- keys(tTree)
  n <- length(teams)
  i <- 1
  
  while (i <= n) {
    teamName <- teams[i]
    team <- tTree[[teamName]]
    tTree[teamName] <- f(team, rOptions)
    i <- i + 1
  }
  
  tTree
}

constructStrPrereqs <- function(rOptions, game, gamePrev, tTree) {
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]

  if (is.null(homeTeam)) {
    print(homeTeamName)
  }
  
  if (is.null(awayTeam)) {
    print(awayTeamName)
  }

  game <- updateGamePreRate(game, rOptions, tTree, homeTeam, awayTeam)
  strPrereqs <- list(game=game, tTree=tTree)
  strPrereqs
}

updateStr <- function(strPrereqs, rOptions) {
  game <- strPrereqs[["game"]]
  strPostNorm <- computeLayerOdm(game$A, game$strNorm,
      rOptions$b, rOptions$c)
  tTree <- strPrereqs[["tTree"]]
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]  
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  strNextNorm <- computeLayerRatings(game, rOptions,
      homeTeam, awayTeam, strPostNorm)
  game <- updateGamePostRate(game, rOptions, strNextNorm)
  tTree[homeTeamName] <- updateTeam(homeTeam, game, 1)
  tTree[awayTeamName] <- updateTeam(awayTeam, game, 2)
  updateStrData <- list(tTree=tTree, game=game)
  updateStrData
}
