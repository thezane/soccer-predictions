newGameHypo <- function(homeTeamName, awayTeamName, contestType,
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
    isQualifier=grepl("-Q", contestType)
  )

  gameHypo$meanGoals <- computeHypoGoals(gameHypo, homeTeam, awayTeam,
      rOutput$meanGoalsMap)

  class(gameHypo) <- "Game"
  gameHypo
}
