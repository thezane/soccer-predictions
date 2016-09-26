computeMeanGoals <- function(isQualifier, isPlayOff, existsHA,
      contest, meanGoalsMap) {
  meanGoals <- c(1, 1)

  if (isQualifier && !isPlayOff) {
    meanGoals <- c(
        meanGoalsMap[[paste("-Q-", getQualifierLocation(contest),
            "-Home", sep="")]],
        meanGoalsMap[[paste("-Q-", getQualifierLocation(contest),
            "-Away", sep="")]])
  }
  else if (existsHA) {
    meanGoals <- c(meanGoalsMap[["-T-Home"]],
        meanGoalsMap[["-T-Away"]])
  }
  else {
    meanGoals <- meanGoalsMap[["-T-Away"]] * meanGoals
  }

  meanGoals
}
