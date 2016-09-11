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

getGameData <- function(games, i, meanGoalsMap) {
  game <- games$T[i, ]

  if (i > 1) {
    nextGame <- games$T[i - 1, ]
  }
  else {
    nextGame <- NULL
  }

  goalsNorm <- normalizeGoals(game, meanGoalsMap)
  gamesData <- list(game=game, nextGame=nextGame, goalsNorm=goalsNorm)
  gamesData
}

updateGames <- function(games, i, game) {
  games$T[i, ] <- game
  games
}

normalizeGoals <- function(T, meanGoalsMap) {
  goalsNorm <- c(T[["HomeGoals"]], T[["AwayGoals"]])
  contest <- T[["Contest"]]

  if (grepl("-Q", contest)) {
    meanGoals <- c(meanGoalsMap[["-Q-Home"]],
        meanGoalsMap[["-Q-Away"]])
  }
  else if (T[["HomeAdvantage"]]) {
    meanGoals <- c(meanGoalsMap[["-T-Home"]],
        meanGoalsMap[["-T-Away"]])
  }
  else {
    meanGoals <- meanGoalsMap[["-T-Away"]] * c(1, 1)
  }

  goalsNorm[1] <- goalsNorm[1] * (meanGoals[2] / meanGoals[1])
  goalsNorm
}
