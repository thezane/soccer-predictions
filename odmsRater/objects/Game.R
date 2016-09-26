newGame <- function(T, i, homeTeamName, awayTeamName,
    gameDate, currentContest) {
  contest <- T[[i, "Contest"]]
  goals <- c(T[[i, "HomeGoals"]], T[[i, "AwayGoals"]])
  zeroesMat <- matrix(0, 2, 2)
  
  game <- list(
    contest=contest,
    gameDate=gameDate,
    gameDateStr=as.character(gameDate),
    year=as.numeric(format(gameDate, "%Y")),
    goals=goals,
    goalsNorm=c(0, 0),
    meanGoals=c(0, 0),
    sse=0,
    A=zeroesMat,
    teamNames=c(homeTeamName, awayTeamName),
    teamStr=zeroesMat,
    strNorm=zeroesMat,
    strNext=zeroesMat,
    strPost=zeroesMat,
    strAgg=c(0, 0),
    strAggNext=c(0, 0),
    teamXP=zeroesMat,
    existsHA=T[[i, "HomeAdvantage"]],
    isQualifier=grepl("-Q", contest),
    isInternational=grepl("(WOC)|(COC)", contest),
    isPlayOff=grepl("-PlayOff", contest),
    isSame=grepl(currentContest, contest),
    isWocG=(contest=="WOC-G"),
    gameNum=0,
    gameRow=i
  )
  
  game$isRelevant <- game$isPlayOff || (!game$isQualifier &&
      (game$isInternational || game$isSame))
  
  class(game) <- "Game"
  game
} 

normalizeGoals <- function(game, meanGoalsMap) {
  goals <- game$goals
  goalsNorm <- goals
  meanGoals <- computeMeanGoals(game$isQualifier, game$isPlayOff,
      game$existsHA, game$contest, meanGoalsMap)
  goalsNorm[1] <- goalsNorm[1] * (meanGoals[2] / meanGoals[1])
  game$goalsNorm <- goalsNorm
  game$meanGoals <- meanGoals
  game$A <- matrix(c(0, goalsNorm[2], goalsNorm[1], 0), 2, 2, TRUE)
  game
}

updateGamePreRate <- function(game, rOptions, homeTeam, awayTeam) {
  ks <- rOptions$ks
  strBetas <- rOptions$model$strBetas
  gameDate <- game$gameDate
  homeTeamStrs <- computeTeamStrs(homeTeam, rOptions)
  awayTeamStrs <- computeTeamStrs(awayTeam, rOptions)
  game$teamStr <- matrix(c(homeTeamStrs[["teamStr"]],
      awayTeamStrs[["teamStr"]]), 2, 2, TRUE)
  game$strNorm <- matrix(c(homeTeamStrs[["strNorm"]],
      awayTeamStrs[["strNorm"]]), 2, 2, TRUE)
  strNorm <- game$strNorm
  strBetas[2] <- -strBetas[2]
  game$strAgg <- c(homeTeamStrs[["strAgg"]], awayTeamStrs[["strAgg"]])

  if (game$isQualifier) {
    k <- ks[1]
  }
  else {
    k <- ks[2]
  }
  
  game$teamXP <- c(computeXP(homeTeam, gameDate, k),
      computeXP(awayTeam, gameDate, k))
  game
}


updateGamePostRate <- function(game, rOptions, strPost) {
  strBetas <- rOptions$model$strBetas
  game$strPost <- strPost  
  alphas <- 1 / (1 + game$teamXP)
  game$strNext <- alphas * strPost + (1 - alphas) * game$teamStr
  game$strNextNorm <- computeStrNorm(game$strNext)
  strNextNorm <- game$strNextNorm
  strBetas[2] <- -strBetas[2]
  game$strAggNext <- c(strNextNorm[1, ] %*% strBetas,
      strNextNorm[2, ] %*% strBetas)
  game
}

updateSSE <- function(game, sse) {
  game$sse <- sse
  game
}
