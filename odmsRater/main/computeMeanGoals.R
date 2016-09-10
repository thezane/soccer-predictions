computeMeanGoals <- function(game, meanGoalsMap) {
  meanGoals <- c(1, 1)

  if (game$isQualifier) {
    meanGoals <- c(meanGoalsMap[["-Q-Home"]],
        meanGoalsMap[["-Q-Away"]])
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
