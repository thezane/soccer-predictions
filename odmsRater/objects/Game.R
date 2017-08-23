new.Game <- function(T, i, rOptions, homeTeamName, awayTeamName,
    gameDate) {
  contest <- T[[i, "Contest"]]
  type <- T[[i, "Type"]]
  goals <- c(T[[i, "HomeGoals"]], T[[i, "AwayGoals"]])
  zeroesMat <- matrix(0, 2, 2)
  
  game <- list(
    gameNum=1,
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
  )
  
  game$dataset <- assignDataset.Game(game, rOptions$currentDate)
  game$isRelevant <- computeRelevance.Game(game)
  game$weight <- computeWeight.Game(game, rOptions)

  class(game) <- "Game"
  game
}

assignDataset.Game <- function(game, currentDate) {
  if (game$gameDate <= currentDate) {
    dataset <- "training"
  }
  else {
    dataset <- "validation"
  }

  dataset
}

computeRelevance.Game <- function(game) {
  (!game$isFriendly && !game$isQualifier) || game$isPlayoff
}

computeWeight.Game <- function(game, rOptions) {
  wTree = rOptions$wTree
	
  if (game$isWorldCup && !game$isQualifier && !game$isFriendly) {
    weight = wTree[["very high"]]
  }
  else if (game$isMajor && !game$isQualifier && !game$isFriendly) {
    weight = wTree[["high"]]
  }
  else if (!game$isFriendly) {
    weight = wTree[["moderate"]]
  }
  else {
    weight = wTree[["very low"]]
  }

  weight
}

computeReliability.Game <- function(game, rOptions,
    homeTeam, awayTeam) {
  n <- rOptions$minUpdatesUntilReliable
  reliability <- c(1, 1)
  
  if (homeTeam$numUpdates < n && awayTeam$numUpdate >= n) {
    reliability[2] <- min(1, (1 + homeTeam$numUpdates) / (1 + n))
  }
  else if (awayTeam$numUpdates < n && homeTeam$numUpdates >= n) {
    reliability[1] <- min(1, (1 + awayTeam$numUpdates) / (1 + n))
  }

  reliability
}

# Adjust goals scored by teams with home advantage.
normalizeGoals.Game <- function(game, rOptions) {
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
updatePreRate.Game <- function(game, rOptions, tTree,
    homeTeam, awayTeam) {
  game$reliability <- computeReliability.Game(game, rOptions,
      homeTeam, awayTeam)
  homeTeamStrs <- getStrs.Team(homeTeam, rOptions, tTree)
  awayTeamStrs <- getStrs.Team(awayTeam, rOptions, tTree)
  game$strNorm <- matrix(c(homeTeamStrs[["strNorm"]],
      awayTeamStrs[["strNorm"]]), 2, 2, TRUE)
  game$strNormBeta <- rOptions$strBeta * game$strNorm
  game$strAgg <- c(homeTeamStrs[["strAgg"]], awayTeamStrs[["strAgg"]])
  game
}

# Update team ratings after game.
updatePostRate.Game <- function(game, rOptions, strNextNorm) {
  game$strNextNorm <- strNextNorm
  game$strNextNormBeta <- rOptions$strBeta * strNextNorm
  game$strAggNext <- c(game$strNextNorm[1, ] %*% rOptions$strBetas,
      game$strNextNorm[2, ] %*% rOptions$strBetas)
  game
}

computeSSE.Game <- function(game, resultExpected, resultActual) {
  game$sse <- sum((resultExpected - resultActual) ^ 2)
  game
}
