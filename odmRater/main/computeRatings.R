computeRatings <- function(rData, currentContest) {
  games <- rData[["games"]]
  tTree <- rData[["tTree"]]
  meanGoalsMap <- constructMeanGoalsMap(games$T)
  gameDatePrev <- NULL
  k <- 1 / (1.25 * 365)
  c <- 0.01
  tolScale <- 0.01
  numTeams <- length(tTree)
  A <- matrix(0, numTeams, numTeams)
  i <- games$numGames

  while (i >= 1) {
    gamesData <- getGameData(games, i, meanGoalsMap)
    game <- gamesData[["game"]]
    gameDate <- as.Date(game[["Date"]])
    A <- constructStrPrereqs(A, gamesData, tTree,
        gameDate, gameDatePrev, k)
    v <- computeStr(A, numTeams, c, tolScale)
    updateStrData <- updateStr(v, gamesData, tTree, currentContest)
    games <- updateGames(games, i, updateStrData[["game"]])
    tTree <- updateStrData[["tTree"]]
    gameDatePrev <- gameDate

    i <- i - 1
  }

  rData[["A"]] <- A
  rData[["games"]] <- games
  rData[["endDate"]] <- gameDatePrev
  rData
}

constructStrPrereqs <- function(A, gamesData, tTree,
      gameDate, gameDatePrev, k) {
  if (!is.null(gameDatePrev)) {
    t <- as.numeric(gameDate) - as.numeric(gameDatePrev)
  }
  else {
    t <- 0
  }

  A <- computeExpDecay(t, k, A)
  game <- gamesData[["game"]]
  goalsNorm <- gamesData[["goalsNorm"]]
  homeTeamName <- game[["HomeTeam"]]
  awayTeamName <- game[["AwayTeam"]]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  homeTeamI <- homeTeam$i
  awayTeamI <- awayTeam$i
  A[homeTeamI, awayTeamI] <- A[homeTeamI, awayTeamI] + goalsNorm[2]
  A[awayTeamI, homeTeamI] <- A[awayTeamI, homeTeamI] + goalsNorm[1]
  A
}
