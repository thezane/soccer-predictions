newGame <- function(T, i, homeTeamName, awayTeamName,
    currentDate, gameDate) {
  contest <- T[[i, "Contest"]]
  type <- T[[i, "Type"]]
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
    reliability=c(1, 1),
    teamNames=c(homeTeamName, awayTeamName),
    strNorm=zeroesMat,
    strNormBeta=zeroesMat,
    strNextNorm=zeroesMat,
    strNextNormBeta=zeroesMat,
    strAgg=c(0, 0),
    strAggNext=c(0, 0),
    existsHA=T[[i, "HomeAdvantage"]],
    sse=0,

    # Contest
    contest=contest,
    isMajor=contest != "AFC Challenge Cup" &&
        contest != "Copa Centroamericana" &&
        contest != "Caribbean Cup",
    type=type,
    isFriendly=type=="Friendlies",
    isQualifier=grepl("Qualifiers", type),
    isPlayoff=grepl("Playoff", type),
    isWorldCup=grepl("World Cup", contest),
    isRelevant=FALSE,
    weight=0
  )
  
  if (gameDate <= currentDate) {
    game$dataset <- "training"
  }
  else {
    game$dataset <- "validation"
  }
  
  game$isRelevant <- computeRelevance(game)
  game$weight <- computeWeight(game)

  class(game) <- "Game"
  game
}

computeRelevance <- function(game) {
  (!game$isFriendly && !game$isQualifier) || game$isPlayoff
}

computeWeight <- function(game) {
  if (game$isWorldCup && !game$isQualifier && !game$isFriendly) {
    weight = 1
  }
  else if (game$isMajor && !game$isQualifier && !game$isFriendly) {
    weight = 5 / 6
  }
  else if (!game$isFriendly) {
    weight = 2 / 3
  }
  else {
    weight = 1 / 3
  }

  weight
}

computeReliability <- function(game, homeTeam, awayTeam) {
  minUpdates <- 10
  reliability <- c(1, 1)
  
  if (homeTeam$numUpdates < minUpdates &&
      awayTeam$numUpdate >= minUpdates) {
    reliability[2] <- min(1,
        (1 + homeTeam$numUpdates) / (1 + minUpdates))
  }
  else if (awayTeam$numUpdates < minUpdates &&
      homeTeam$numUpdates >= minUpdates) {
    reliability[1] <- min(1,
        (1 + awayTeam$numUpdates) / (1 + minUpdates))
  }

  reliability <- reliability
  reliability
}

# Adjust goals scored by teams with home advantage.
normalizeGoals <- function(game, rOptions) {
  goals <- game$goals
  goalsNorm <- goals
  meanGoals <- computeMeanGoals(game$existsHA, rOptions)
  goalsNorm[1] <- (meanGoals[2] / meanGoals[1]) * goalsNorm[1]
  game$goalsNorm <- goalsNorm
  game$meanGoals <- meanGoals
  game$A <- matrix(c(0, goalsNorm[2], goalsNorm[1], 0), 2, 2, TRUE)
  game
}

# Construct ratings matrix before game.
updateGamePreRate <- function(game, rOptions, tTree,
    homeTeam, awayTeam) {
  homeTeamStrs <- getTeamStrs(homeTeam, rOptions, tTree)
  awayTeamStrs <- getTeamStrs(awayTeam, rOptions, tTree)
  game$strNorm <- matrix(c(homeTeamStrs[["strNorm"]],
      awayTeamStrs[["strNorm"]]), 2, 2, TRUE)
  game$strNormBeta <- rOptions$strBeta * game$strNorm
  game$strAgg <- c(homeTeamStrs[["strAgg"]], awayTeamStrs[["strAgg"]])
  game
}

# Update team ratings after game.
updateGamePostRate <- function(game, rOptions, strNextNorm) {
  game$strNextNorm <- strNextNorm
  game$strNextNormBeta <- rOptions$strBeta * strNextNorm
  game$strAggNext <- c(game$strNextNorm[1, ] %*% rOptions$strBetas,
      game$strNextNorm[2, ] %*% rOptions$strBetas)
  game
}

computeSSE <- function(game, resultExpected, resultActual) {
  game$sse <- sum((resultExpected - resultActual) ^ 2)
  game
}
