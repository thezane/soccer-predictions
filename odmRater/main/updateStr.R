updateStr <- function(v, gamesData, tTree, currentContest) {
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
  goals <- c(game[["HomeGoals"]], game[["AwayGoals"]])
  strAgg <- c(game[["HomeStrAgg"]], game[["AwayStrAgg"]])

  if (isRelevant(game, currentContest)) {
	strAgg[1] <- strAgg[1] * gamesData[["hA"]]
    game[["IsCorrect"]] <-
        (goals[1] < goals[2] && strAgg[1] < strAgg[2]) ||
        (goals[1] > goals[2] && strAgg[1] > strAgg[2])
  }

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
  game[[paste(prefix, "StrAgg", sep="")]] <- team$strAgg
  game[[paste(prefix, "StrAggNext", sep="")]] <- team$strAggNext
  game
}

isRelevant <- function(game, currentContest) {
  contest <- game[["Contest"]]
  isQualifier <- grepl("-Q", contest)
  isInternational=grepl("(WOC)|(COC)", contest)
  isPlayOff <- grepl("-P", contest)
  isSame <- grepl(currentContest, contest)
  isRelevant <- isPlayOff ||
      (!isQualifier && (isInternational || isSame))
  isRelevant
}
