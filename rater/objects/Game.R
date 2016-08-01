newGame <- function(T, i, homeTeamName, awayTeamName, gameDate) {
  contest <- T[[i, "Contest"]]
  zeroesRow <- c(0, 0)
  game <- list(
    contest=contest,
    gameDate=gameDate,
    year=as.numeric(format(gameDate, "%Y")),
    goals=c(T[[i, "HomeGoals"]], T[[i, "AwayGoals"]]),
    goalsNorm=zeroesRow,
    teamNames=c(homeTeamName, awayTeamName),
    teamStr=zeroesRow,
    teamStrNext=zeroesRow,
    teamStrPost=zeroesRow,
    teamXP=0,
    existsHomeAdvantage=T[[i, "HomeAdvantage"]],
    isQualifier=grepl("-Q", contest),
    gameNumber=0,
    gameRow=i
  )
  class(game) <- "Game"
  game
} 
