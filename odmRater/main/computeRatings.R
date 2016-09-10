computeRatings <- function(rOutput) {
  games <- rOutput[["games"]]
  tTree <- rOutput[["tTree"]]
  meanGoalsMap <- constructMeanGoalsMap(games$T)
  numTeams <- length(tTree)
  c <- 0.01
  tolScale <- 0.01
  A <- matrix(c, numTeams, numTeams)
  i <- games$numGames

  while (i >= 1) {
    gamesData <- getGameData(games, i, meanGoalsMap)
    A <- constructStrPrereqs(A, gamesData, tTree)
    v <- scaleRating(A, numTeams, tolScale)
    updateStrData <- updateStr(v, gamesData, tTree)
    games <- updateT(games, i, updateStrData[["game"]])
    tTree <- updateStrData[["tTree"]]
    i <- i - 1
  }

  rOutput[["A"]] <- A
  rOutput
}

constructStrPrereqs <- function(A, gamesData, tTree) {
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

updateStr <- function(v, gamesData, tTree) {
  x <- v[["x"]]
  y <- v[["y"]]
  game <- gamesData[["game"]]
  nextGame <- gamesData[["nextGame"]]
  homeTeamName <- game[["HomeTeam"]]
  awayTeamName <- game[["AwayTeam"]]
  homeTeam <- updateTeam(tTree[[homeTeamName]], x, y)
  awayTeam <- updateTeam(tTree[[awayTeamName]], x, y)
  tTree[[homeTeamName]] <- homeTeam
  tTree[[awayTeamName]] <- awayTeam
  game <- updateGame(game, homeTeam, TRUE)
  game <- updateGame(game, awayTeam, FALSE)

  if (!is.null(nextGame)) {
    homeTeamName <- nextGame[["HomeTeam"]]
    awayTeamName <- nextGame[["AwayTeam"]]
    tTree[[homeTeamName]] <- updateStrNext(tTree[[homeTeamName]], x, y)
    tTree[[awayTeamName]] <- updateStrNext(tTree[[awayTeamName]], x, y)
  }

  updateStrData <- list(game=game, tTree=tTree)
  updateStrData
}

updateGame <- function(game, team, isHome) {
  if (isHome) {
    prefix <- "Home"
  }
  else {
    prefix <- "Away"
  }

  game[[paste(prefix, "Attack", sep="")]] <- team$teamStr[1]
  game[[paste(prefix, "Defense", sep="")]] <- team$teamStr[2]
  game[[paste(prefix, "AttackNext", sep="")]] <- team$strNext[1]
  game[[paste(prefix, "DefenseNext", sep="")]] <- team$strNext[2] 
  game
}
