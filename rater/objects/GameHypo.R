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
    isQualifier=grepl("-Q", contestType)
  )

  class(gameHypo) <- "Game"
  hAData <- rOutput$hA$hAData

  if (gameHypo$isQualifier) {
    meanGoals <- c(hAData[["qHomeMeanGoals"]],
        hAData[["qAwayMeanGoals"]])
  }
  else if (gameHypo$existsHA) {
    meanGoals <- c(hAData[["tHomeMeanGoals"]],
        hAData[["tAwayMeanGoals"]])
  }
  else {
    meanGoals <- c(hAData[["tAwayMeanGoals"]],
        hAData[["tAwayMeanGoals"]])
  }

  gameHypo$meanGoals <- meanGoals
  gameHypo
}
