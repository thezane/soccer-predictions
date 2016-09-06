newGameHypo <- function(homeTeamName, awayTeamName, contestType,
    location, rData) {
  rOutput <- rData[["rOutput"]]
  tTree <- rOutput$tTree
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]

  gameHypo <- list(
    rData=rData,
    teamNames=c(homeTeamName, awayTeamName),
    meanGoals <- rOutput$meanGoalsMap(contestType),
    strNorm=log(matrix(c(homeTeam$teamStr, awayTeam$teamStr),
        2, 2, TRUE)),
    existsHA=homeTeamName==location,
    isQualifier=grepl("-Q", contestType)
  )

  class(gameHypo) <- "Game"
  gameHypo
}
