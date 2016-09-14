computeHypoGoals <- function(gameHypo, homeTeam, awayTeam,
    meanGoalsMap) {
  meanGoals <- c(1, 1)

  if (gameHypo$isQualifier) {
    meanGoals <- c(
        meanGoalsMap[[paste("-Q-Home-", homeTeam$fName, sep="")]],
        meanGoalsMap[[paste("-Q-Away-", awayTeam$fName, sep="")]])
  }
  else if (gameHypo$existsHA) {
    meanGoals <- c(meanGoalsMap[["-T-Home"]],
        meanGoalsMap[["-T-Away"]])
  }
  else {
    meanGoals <- meanGoalsMap[["-T-Away"]] * meanGoals
  }

  meanGoals
}
