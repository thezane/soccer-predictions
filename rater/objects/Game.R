newGame <- function(T, i, homeTeamName, awayTeamName, tTree, gameDate,
    currentContest) {
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
    teamXP=zeroesMat,
    existsHA=T[[i, "HomeAdvantage"]],
    isQualifier=grepl("-Q", contest),
    isInternational=grepl("(WOC)|(COC)", contest),
    isCoc=grepl("COC", contest),
    isWocG=(contest=="WOC-G"),
    gameNum=0,
    gameRow=i
  )
  
  game$isRelevant <- !game$isQualifier &&
        (game$isInternational || grepl(currentContest, game$contest))
  
  class(game) <- "Game"
  game
} 

normalizeGoals <- function(game, meanGoalsMap) {
  goals <- game$goals
  goalsNorm <- goals
  meanGoals <- computeMeanGoals(game, meanGoalsMap)

  if (meanGoals[1] != meanGoals[2]) {
    goalsNorm[1] <- goalsNorm[1] * (meanGoals[2] / meanGoals[1])
  }

  game$meanGoals <- meanGoals
  game$goalsNorm <- goalsNorm
  game$A <- matrix(c(0, goalsNorm[2], goalsNorm[1], 0), 2, 2, TRUE)
  game
}

updateGamePreRate <- function(game, rOptions, homeTeam, awayTeam) {
  fTree <- rOptions$fTree
  ks <- rOptions$ks
  gameDate <- game$gameDate
  game$teamStr <- matrix(c(computeTeamStr(homeTeam, fTree),
      computeTeamStr(awayTeam, fTree)), 2, 2, TRUE)
  game$strNorm <- computeStrNorm(game$teamStr)
  isKQ <- game$isQualifier || game$isCoc
  k <- as.numeric(isKQ) * ks[1] + as.numeric(!isKQ) * ks[2]
  game$teamXP <- c(computeXP(homeTeam, gameDate, k),
      computeXP(awayTeam, gameDate, k))
  game
}


updateGamePostRate <- function(game, strPost) {
  game$strPost <- strPost
  alphas <- 1 / (1 + game$teamXP)
  game$strNext <- alphas * strPost + (1 - alphas) * game$teamStr
  game
}

updateSSE <- function(game, sse) {
  game$sse <- sse
  game
}
