newGameHypo <- function(homeTeamName, awayTeamName, location, rData) {
  rOptions <- rData[["rOptions"]]
  rOutput <- rData[["rOutput"]]
  tTree <- rOutput$tTree
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]

  gameHypo <- list(
    teamNames=c(homeTeamName, awayTeamName),
    strNormBeta=matrix(c(homeTeam$strNormBeta, awayTeam$strNormBeta),
        2, 2, TRUE),
    strAgg=c(homeTeam$strAgg, awayTeam$strAgg),
    existsHA=homeTeamName %in% location
  )

  gameHypo$meanGoals <- computeMeanGoals(gameHypo$existsHA, rOptions)

  class(gameHypo) <- "Game"
  gameHypo
}
