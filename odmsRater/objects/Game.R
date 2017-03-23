newGame <- function(T, i, homeTeamName, awayTeamName,
    gameDate) {
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
    isFriendly=contest=="Friendlies",
    isQualifier=grepl("-Q", contest),
    isPlayOff=grepl("-PlayOff", contest),
    isWocG=(contest=="WOC-G"),
    gameNum=0,
    gameRow=i
  )
  
  game$isRelevant <- (!game$isFriendly && !game$isQualifier) ||
          game$isPlayOff
  
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

# Update team experience and construct ratings matrix before game.
updateGamePreRate <- function(game, rOptions, homeTeam, awayTeam) {
  strBetas <- rOptions$strBetas
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
  game$teamXP <- c(computeXP(homeTeam, gameDate, rOptions$k),
      computeXP(awayTeam, gameDate, rOptions$k))
  game
}

# Update team ratings after game.
updateGamePostRate <- function(game, rOptions, strPost) {
  strBetas <- rOptions$strBetas
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
