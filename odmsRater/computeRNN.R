computeRNN <- function(rOptions, rOutput) {
  rOutput <- resetRatingsOutput(rOutput)
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  gi <- reset(gi)
  gamePrev <- NULL
  
  while (hasNextEvent(gi)) {
    eventData <- nextEvent(gi)
    gi <- eventData[["gi"]]
    event <- eventData[["event"]]
    
    if (class(event) == "Game") {
      game <- event
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
    }
    else if (class(event) == "Change") {
      change <- event
      tTree <- handleChange(change, tTree)
    }
  }
  
  rOutput <- updateTotalCosts(rOutput, rOptions)
  rOutput$tTree <- tTree
  rOutput$gTree <- gTree
  rOutput$gi <- gi
  rOutput
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
      rOptions$b, rOptions$c, rOptions$odmIter)
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
