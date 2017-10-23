new.Game <- function(homeTeamName, awayTeamName, existsHa, rOptions,
    gameDate=NULL, contest=NULL, goals=NULL, cTree=NULL, i=NULL) {
  zeroesMat <- matrix(0, 2, 2)
  
  game <- list(
    gameNum=1,
    gameRow=i,

    # Date
    gameDate=gameDate,
    gameDateStr=as.character(gameDate),
    year=as.numeric(format(gameDate, "%Y")),

    # Result
    A=zeroesMat,
    Ps=c(0, 0, 0),
    goals=goals,
    goalsOdm=goals,
    goalsOutput=goals,
    outcome=as.numeric(c(goals[1] > goals[2],
        goals[1] == goals[2], goals[1] < goals[2])),
    pGoalsMatSize=rOptions$pGoalsMatSizeBase + max(goals),

    # Ratings
    computeRatings=TRUE,
    existsHa=existsHa,
    reliability=c(1, 1),
    sse=0,
    strNorm=zeroesMat,
    strNormBeta=zeroesMat,
    strNextNorm=zeroesMat,
    strNextNormBeta=zeroesMat,
    strAgg=c(0, 0),
    strAggNext=c(0, 0),
    teamNames=c(homeTeamName, awayTeamName),

    # Contest
    contest=contest
  )
  
  if (!is.null(cTree)) {
      game$dataset <- assignDataset.Game(game, rOptions$currentDate)
      game$isRelevant <- computeRelevance.Game(game, cTree)
      game$weightContest <- computeWeight.Game(game, rOptions, cTree)
  }

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

computeRelevance.Game <- function(game, cTree) {
  contestData <- cTree[[game$contest]]
  contestData[["relevance"]] == "high"
}

computeWeight.Game <- function(game, rOptions, cTree) {
  wTree = rOptions$wTree
  contestData <- cTree[[game$contest]]
  rOptions$wTree[[contestData[["weight"]]]]
}

computeReliability.Game <- function(game, rOptions,
    homeTeam, awayTeam) {
  n <- rOptions$minUpdatesUntilReliable
  reliability <- c(1, 1)
  
  # Home team updates rating cautiously because away team played too
  # few games
  if (awayTeam$numUpdates < n) {
    reliability[1] <- min(1, (1 + awayTeam$numUpdates) / (1 + n))
  }
  
  # Away team updates rating cautiously because home team played too
  # few games
  if (homeTeam$numUpdates < n) {
    reliability[2] <- min(1, (1 + homeTeam$numUpdates) / (1 + n))
  }

  reliability
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
