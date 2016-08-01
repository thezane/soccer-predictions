rateTeams <- function(rOptions, rOutput) {
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  tTree <- resetRatings(tTree)
  gi <- reset(gi)
  
  while (hasNext(gi)) {
    gameData <- nextGame(gi)
    strPrereqs <- computeStrPrereq(tTree, game, rOptions)
    A <- strPrereqs[["A"]]
    game <- strPrereqs[["game"]]
    updateStrData <- updateStr(tTree, game, A, rOptions)
    rOutput <- updateCost(rOutput, rOptions, game)
    gDateList <- gTree(game$gameDate)
    gDateList[game$gameNum] <- game
    gTree[game$gameData] <- gDateList
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
  }
  else if (awayTeam$xp == 0) {
    awayTeam$teamStr <- fTree[[awayTeam$fName]]
  }
  
  game$teamStr <- matrix(c(homeTeam$teamStr, awayTeam$teamStr), TRUE)
  k <- rOptions$k
  goals <- game$goalsNorm
  A <- matrix(c(0, goals(2), goals(1), 0), 2, 2, TRUE)
  t <- game$gameDate - c(homeTeam$updateDate, awayTeam$updateDate)
  game$teamXP <- expDecay(t, k / 365, c(homeTeam$xp, awayTeam$xp))
  strPrereqs <- list(A=A, game=game)
  strPrereqs
}

updateStr <- function(tTree, game, A, rOptions) {
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamname]]
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
  updateStrData <- list(tTree=tTree, game=game, strPost=strPost)
  updateStrData
}
