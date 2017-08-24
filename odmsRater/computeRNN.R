computeRNN <- function(rOptions, rOutput) {
  rOutput <- reset.RatingsOutput(rOutput)
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  gi <- reset.EventIterator(gi)
  gamePrev <- NULL
  
  while (hasNext.EventIterator(gi)) {
    eventData <- next.EventIterator(gi)
    gi <- eventData[["gi"]]
    event <- eventData[["event"]]
    
    if (class(event) == "Game") {
      game <- event
      game <- normalizeGoals.Game(game, rOptions)
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
      tTree <- handle.Change(change, tTree)
    }
  }
  
  rOutput <- updateTotalCosts.RatingsOutput(rOutput, rOptions)
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

  game <- updatePreRate.Game(game, rOptions, tTree,
      homeTeam, awayTeam)
  strPrereqs <- list(game=game, tTree=tTree)
  strPrereqs
}

updateStr <- function(strPrereqs, rOptions) {
  game <- strPrereqs[["game"]]
  tTree <- strPrereqs[["tTree"]]
  strPostNorm <- computeLayerOdm(game, rOptions)
  strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)
  game <- updatePostRate.Game(game, rOptions, strNextNorm)
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]  
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  tTree[homeTeamName] <- update.Team(homeTeam, game, 1)
  tTree[awayTeamName] <- update.Team(awayTeam, game, 2)
  updateStrData <- list(tTree=tTree, game=game)
  updateStrData
}
