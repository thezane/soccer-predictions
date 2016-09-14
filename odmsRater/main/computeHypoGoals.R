computeHypoGoals <- function(gameHypo, homeTeam, awayTeam,
    meanGoalsMap) {
  meanGoals <- c(1, 1)

  if (game$isQualifier) {
    meanGoals <- c(meanGoalsMap[[paste("-Q-Home-", homeTeam$fName)]],
        meanGoalsMap[[paste("-Q-Away-", awayTeam$fName)]])
  }
  else if (game$existsHA) {
    meanGoals <- c(meanGoalsMap[["-T-Home"]],
        meanGoalsMap[["-T-Away"]])
  }
  else {
    meanGoals <- meanGoalsMap[["-T-Away"]] * meanGoals
  }
  
  meanGoals
}
