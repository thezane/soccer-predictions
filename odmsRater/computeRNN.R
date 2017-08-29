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
      strPrereqs <- constructStrPrereqs(rOptions, game, tTree)
      game <- strPrereqs[["game"]]
      tTree <- strPrereqs[["tTree"]]
      layerOutput <- rOptions$layersComputer(rOptions, game)
      gamePrediction <- layerOutput[["gamePrediction"]]
      strNextNorm <- layerOutput[["strNextNorm"]]
      updateStrData <- updateStr(strPrereqs, rOptions, strNextNorm)
      game <- updateStrData[["game"]]
      tTree <- updateStrData[["tTree"]]
      costData <- updateCost(rOutput, gamePrediction, game, gamePrev)
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

constructStrPrereqs <- function(rOptions, game, tTree) {
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

updateStr <- function(strPrereqs, rOptions, strNextNorm) {
  game <- strPrereqs[["game"]]
  tTree <- strPrereqs[["tTree"]]
  game <- updatePostRate.Game(game, rOptions, strNextNorm)
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]  
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  tTree[homeTeamName] <- update.Team(homeTeam, game, 1)
  tTree[awayTeamName] <- update.Team(awayTeam, game, 2)
  updateStrData <- list(game=game, tTree=tTree)
  updateStrData
}

updateCost <- function(rOutput, gamePrediction, game, gamePrev) {

  if (!is.null(gamePrev) && gamePrev$year < game$year) {
    rOutput <- updateStrMeanCosts.RatingsOutput(rOutput, game$dataset)
  }

  if (game$isRelevant) {
    # Update cost of outcome
    resultExpected <- gamePrediction[["pWinTieLoss"]]
    resultActual <- game$outcome
    game <- computeSSE.Game(game, resultExpected, resultActual)
    game$Ps <- resultExpected
  
    # Update cost of goals
    p <- gamePrediction[["p"]]
    reliability <- game$reliability
    weight <- game$weight
    weightCost <- min(reliability) * weight
    dataset <- game$dataset
    rOutput <- updateGoalsCost.RatingsOutput(rOutput, p, weightCost,
        dataset)
    costData <- list(rOutput=rOutput, game=game)
    costData
  }
  else {
    costData <- list(rOutput=rOutput, game=game)
  }

  costData
}
