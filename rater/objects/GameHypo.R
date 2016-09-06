newGameHypo <- function(homeTeamName, awayTeamName, contestType,
    location, rData) {
  rOutput <- rData[["rOutput"]]
  tTree <- rOutput$tTree
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]

  gameHypo <- list(
    rData=rData,
    teamNames=c(homeTeamName, awayTeamName),
    strNorm=log(matrix(c(homeTeam$teamStr, awayTeam$teamStr),
        2, 2, TRUE)),
    existsHA=homeTeamName==location,
    isQualifier=contestType=="-Q"
  )

  gameHypo$meanGoals <- computeMeanGoals(gameHypo,
      rOutput$meanGoalsMap)

  class(gameHypo) <- "Game"
  gameHypo
}
