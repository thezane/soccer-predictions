newGames <- function(T, currentDate, dateFormat) {
  T["Date"] <- sapply(T[["Date"]],
      function(date, dateFormat.=dateFormat) {
        as.character(as.Date(date, dateFormat))})
  T <- T[T$Date <= currentDate, ]
  colNames = c(
      "HomeStrAgg", "AwayStrAgg",
      "HomeStrAggNext", "AwayStrAggNext",
      "HomeAttack", "HomeDefense",
      "AwayAttack", "AwayDefense",
      "HomeAttackNext", "HomeDefenseNext",
      "AwayAttackNext", "AwayDefenseNext",
      "IsCorrect")
  T[colNames] <- 0

  games <- list(
    T=T,
    colNames=colNames,
    numGames=nrow(T)
  )
  
  class(games) <- "Games"
  games
} 

getGameData <- function(games, i, tTree, meanGoalsMap) {
  game <- games$T[i, ]

  if (i > 1) {
    nextGame <- games$T[i - 1, ]
  }
  else {
    nextGame <- NULL
  }

  goalsNormData <- normalizeGoals(game, tTree, meanGoalsMap)
  goalsNorm <- goalsNormData[["goalsNorm"]]
  hA <- goalsNormData[["hA"]]
  gamesData <- list(game=game, nextGame=nextGame, goalsNorm=goalsNorm,
      hA=hA)
  gamesData
}

updateGames <- function(games, i, game) {
  games$T[i, ] <- game
  games
}

normalizeGoals <- function(T, tTree, meanGoalsMap) {
  goalsNorm <- c(T[["HomeGoals"]], T[["AwayGoals"]])
  contest <- T[["Contest"]]
  isQualifier <- grepl("-Q", contest)
  isPlayOff <- grepl("-P", contest)
  existsHA <- T[["HomeAdvantage"]]
  homeTeam <- tTree[[T[["HomeTeam"]]]]
  awayTeam <- tTree[[T[["AwayTeam"]]]]
  meanGoals <- computeMeanGoals(isQualifier, isPlayOff, existsHA,
      homeTeam, awayTeam, meanGoalsMap)
  goalsNorm[1] <- goalsNorm[1] * (meanGoals[2] / meanGoals[1])
  hA <- meanGoals[1] / meanGoals[2]
  goalsNormData <- list(goalsNorm=goalsNorm, hA=hA)
  goalsNormData
}
