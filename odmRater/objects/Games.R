newGames <- function(T, dateFormat) {
  T["Dates"] <- sapply(T[["Dates"]],
      function(date, dateFormat.=dateFormat) {
        as.Date(date, dateFormat)})
  colNames = c(
      "HomeAttack", "HomeDefense",
      "AwayAttack", "AwayDefense",
      "HomeAttackNext", "HomeDefenseNext",
      "AwayAttackNext", "AwayDefenseNext")
  T[colNames] <- 0

  games <- list(
    T=T,
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

updateT <- function(games, i, game) {
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
