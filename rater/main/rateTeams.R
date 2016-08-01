rateTeams <- function(rOptions, rOutput) {
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  tTree <- resetRatings(tTree)
  gi <- reset(gi)
  i <- 1
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    strPrereqs <- computeStrPrereqs(tTree, game, rOptions)
    A <- strPrereqs[["A"]]
    game <- strPrereqs[["game"]]
    updateStrData <- updateStr(tTree, game, A, rOptions)
    tTree <- updateStrData[["tTree"]]
    game <- updateStrData[["game"]]
    rOutput <- updateCost(rOutput, rOptions, game)
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- game
    gTree[game$gameDateStr] <- gDateList
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
  
  if (homeTeam$xp == 0) {
    homeTeam$teamStr <- fTree[[homeTeam$fName]]
    homeTeam$updateDate <- game$gameDate
  }
  
  if (awayTeam$xp == 0) {
    awayTeam$teamStr <- fTree[[awayTeam$fName]]
    awayTeam$updateDate <- game$gameDate
  }
  
  game$teamStr <- matrix(c(homeTeam$teamStr, awayTeam$teamStr), 2, 2,
      TRUE)
  k <- rOptions$k
  goals <- game$goalsNorm
  A <- matrix(c(0, goals[2], goals[1], 0), 2, 2, TRUE)
  t <- as.numeric(
      game$gameDate - c(homeTeam$updateDate, awayTeam$updateDate))
  game$teamXP <- expDecay(t, k / 365, c(homeTeam$xp, awayTeam$xp))
  strPrereqs <- list(A=A, game=game)
  strPrereqs
}

updateStr <- function(tTree, game, A, rOptions) {
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
