newGame <- function(T, i, homeTeamName, awayTeamName, gameDate) {
  contest <- T[[i, "Contest"]]
  zeroesMat <- matrix(0, 2, 2)
  game <- list(
    contest=contest,
    gameDate=gameDate,
    gameDateStr=as.character(gameDate),
    year=as.numeric(format(gameDate, "%Y")),
    goals=c(T[[i, "HomeGoals"]], T[[i, "AwayGoals"]]),
    goalsNorm=c(0, 0),
    teamNames=c(homeTeamName, awayTeamName),
    teamStr=zeroesMat,
    teamStrNext=zeroesMat,
    teamStrPost=zeroesMat,
    teamXP=c(0, 0),
    existsHA=T[[i, "HomeAdvantage"]],
    isQualifier=grepl("-Q", contest),
    gameNum=0,
    gameRow=i
  )
  class(game) <- "Game"
  game
} 
