rateTeams <- function(rOptions, rOutput) {
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  tTree <- resetRatings(tTree, rOptions)
  gi <- reset(gi)
  gamePrev <- NULL
  i <- 1
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    strPrereqs <- computeStrPrereqs(tTree, game, rOptions)
    updateStrData <- updateStr(strPrereqs, rOptions)
    tTree <- updateStrData[["tTree"]]
    game <- updateStrData[["game"]]
    rOutput <- updateCost(rOutput, game, gamePrev)
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- game
    gTree[game$gameDateStr] <- gDateList
    gamePrev <- game
    i <- i + 1
  }
  
  rOutput$tTree <- tTree
  rOutput$gTree <- gTree
  rOutput$gi <- gi
  rOutput
}

computeStrPrereqs <- function(tTree, game, rOptions) {
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  fTree <- rOptions$fTree 

  if (!homeTeam$isUpdated) {
    homeTeam$isUpdated <- TRUE
    homeTeam$teamStr <- fTree[[homeTeam$fName]]
    homeTeam$updateDate <- game$gameDate
    tTree[homeTeamName] <- homeTeam
  }
  
  if (!awayTeam$isUpdated) {
    awayTeam$isUpdated <- TRUE
    awayTeam$teamStr <- fTree[[awayTeam$fName]]
    awayTeam$updateDate <- game$gameDate
    tTree[awayTeamName] <- awayTeam
  }
  
  game$teamStr <- matrix(c(homeTeam$teamStr, awayTeam$teamStr), 2, 2,
      TRUE)
  goals <- game$goalsNorm
  A <- matrix(c(0, goals[2], goals[1], 0), 2, 2, TRUE)
  t <- as.numeric(
      game$gameDate - c(homeTeam$updateDate, awayTeam$updateDate))
  game$teamXP <- expDecay(t, rOptions$k / 365,
      c(homeTeam$xp, awayTeam$xp))
  strPrereqs <- list(A=A, game=game, tTree=tTree)
  strPrereqs
}

updateStr <- function(strPrereqs, rOptions) {
  A <- strPrereqs[["A"]]
  game <- strPrereqs[["game"]]
  tTree <- strPrereqs[["tTree"]]
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  teamStr <- game$teamStr
  strPost <- computeStr(A, teamStr, rOptions$c,
      rOptions$tolRel, rOptions$tolScale)
  game$teamStrPost <- strPost
  teamXP <- game$teamXP
  alphas <- 1 / (1 + teamXP)
  strNext <- computeStrNext(teamStr, strPost, alphas)
  game$teamStrNext <- strNext
  homeTeam$teamStr <- strNext[1, ]
  awayTeam$teamStr <- strNext[2, ]
  homeTeam$xp <- teamXP[1] + 1
  awayTeam$xp <- teamXP[2] + 1
  homeTeam$updateDate <- game$gameDate
  awayTeam$updateDate <- game$gameDate
  tTree[homeTeamName] <- homeTeam
  tTree[awayTeamName] <- awayTeam
  updateStrData <- list(tTree=tTree, game=game)
  updateStrData
}
