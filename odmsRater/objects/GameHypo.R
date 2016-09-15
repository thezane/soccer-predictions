newGameHypo <- function(homeTeamName, awayTeamName, contest,
    location, rData) {
  rOutput <- rData[["rOutput"]]
  tTree <- rOutput$tTree
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]

  gameHypo <- list(
    rData=rData,
    teamNames=c(homeTeamName, awayTeamName),
    strNorm=matrix(c(homeTeam$strNorm, awayTeam$strNorm),
        2, 2, TRUE),
    strAgg=c(homeTeam$strAgg, awayTeam$strAgg),
    existsHA=homeTeamName==location,
    isQualifier=grepl("-Q", contest),
    isPlayOff=grepl("-P", contest)
  )

  gameHypo$meanGoals <- computeMeanGoals(gameHypo$isQualifier,
      gameHypo$isPlayOff, gameHypo$existsHA, homeTeam, awayTeam,
      rOutput$meanGoalsMap)

  class(gameHypo) <- "Game"
  gameHypo
}
