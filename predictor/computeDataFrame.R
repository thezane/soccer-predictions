computeDataFrame <-function(matches, meanGoalsMap) {
  meanGoals <- apply(matches, 1, mapContestToMeanGoals,
      meanGoalsMap=meanGoalsMap)
  meanGoals <- t(meanGoals)
  matches[["HomeMeanGoals"]] <- meanGoals[, 1]
  matches[["AwayMeanGoals"]] <- meanGoals[, 2]
  matches <- matches[order(rev(matches$Date)), ] 
  matches
}

mapContestToMeanGoals <- function(matchesRow, meanGoalsMap) {
  meanGoals <- vector(mode="double", 2)
  contest <- matchesRow["Contest"]
  
  if (contest == "EUC-Q" || contest == "WOC-Q") {
    meanGoals[1] <- meanGoalsMap[["qualifierHome"]]
    meanGoals[2] <- meanGoalsMap[["qualifierAway"]]
  }
  else if (contest == "EUC-G") {
    meanGoals[1] <- meanGoalsMap[["group"]]
    meanGoals[2] <- meanGoals[1]
  }
  else {
    meanGoals[1] <- meanGoalsMap[["knockout"]]
    meanGoals[2] <- meanGoals[1]
  }
  
  meanGoals
}
