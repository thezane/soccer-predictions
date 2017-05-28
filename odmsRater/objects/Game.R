newGame <- function(T, i, homeTeamName, awayTeamName,
    gameDate) {
  contest <- T[[i, "Contest"]]
  goals <- c(T[[i, "HomeGoals"]], T[[i, "AwayGoals"]])
  zeroesMat <- matrix(0, 2, 2)
  
  game <- list(
    gameNum=0,
    gameRow=i,

    # Date
    gameDate=gameDate,
    gameDateStr=as.character(gameDate),
    year=as.numeric(format(gameDate, "%Y")),

    # Result
    goals=goals,
    goalsNorm=c(0, 0),
    meanGoals=c(0, 0),
    outcome=as.numeric(c(goals[1] > goals[2],
        goals[1] == goals[2], goals[1] < goals[2])),
    Ps=c(0, 0, 0),
    A=zeroesMat,

    # Ratings
    teamNames=c(homeTeamName, awayTeamName),
    strNorm=zeroesMat,
    strNextNorm=zeroesMat,
    strAgg=c(0, 0),
    strAggNext=c(0, 0),
    existsHA=T[[i, "HomeAdvantage"]],
    sse=0,

    # Contest
    contest=contest,
    isFriendly=contest=="Friendlies",
    isQualifier=grepl("-Q", contest),
    isPlayOff=grepl("-PlayOff", contest),
    isGroup=grepl("-G", contest),
    isCoc=grepl("COC", contest),
    isWoc=grepl("WOC", contest),
    isWocG=(contest=="WOC-G"),
    isWocK=(contest=="WOC-K"),
    weight=0.8
  )
  
  game$isRelevant <- game$isPlayOff ||
          (!game$isFriendly && !game$isQualifier)
  
  if (game$isWocG || game$isWocK) {
    game$weight = 1
  }
  
  class(game) <- "Game"
  game
}

# Adjust goals scored by teams with home advantage.
normalizeGoals <- function(game, rOptions) {
  goals <- game$goals
  goalsNorm <- goals
  meanGoals <- computeMeanGoals(game$existsHA, rOptions)
  goalsNorm[1] <- goalsNorm[1] * (meanGoals[2] / meanGoals[1])
  game$goalsNorm <- goalsNorm
  game$meanGoals <- meanGoals
  game$A <- matrix(c(0, goalsNorm[2], goalsNorm[1], 0), 2, 2, TRUE)
  game
}

# Construct ratings matrix before game.
updateGamePreRate <- function(game, rOptions, homeTeam, awayTeam) {
  homeTeamStrs <- getTeamStrs(homeTeam, rOptions)
  awayTeamStrs <- getTeamStrs(awayTeam, rOptions)
  game$strNorm <- matrix(c(homeTeamStrs[["strNorm"]],
      awayTeamStrs[["strNorm"]]), 2, 2, TRUE)
  game$strAgg <- c(homeTeamStrs[["strAgg"]], awayTeamStrs[["strAgg"]])  
  game
}

# Update team ratings after game.
updateGamePostRate <- function(game, rOptions, strNextNorm) {
  game$strNextNorm <- strNextNorm
  game$strAggNext <- c(game$strNextNorm[1, ] %*% rOptions$strBetas,
      game$strNextNorm[2, ] %*% rOptions$strBetas)
  game
}

computeSSE <- function(game, resultExpected, resultActual) {
  game$sse <- sum((resultExpected - resultActual) ^ 2)
  game
}
