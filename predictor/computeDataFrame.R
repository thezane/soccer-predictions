computeDataFrame <-function(matches, meanGoalsMap) {
  meanGoals <- apply(matches, MARGIN=1, mapContestToMeanGoals,
      meanGoalsMap=meanGoalsMap)
  meanGoals <- t(meanGoals)
  matches[["HomeMeanGoals"]] <- meanGoals[, 1]
  matches[["AwayMeanGoals"]] <- meanGoals[, 2]
  matches <- matches[rev(order(as.Date(matches[, "Date"],
      format="%Y/%m/%d"))), ]
  matchContests <- lapply(matches[, "Contest"], getGeneralContest)
  tMatches <- matches[matchContests != "qualifier", ]
  rownames(tMatches) <- 1:nrow(tMatches)
  matchFrames <- list("Matches"=matches, "TMatches"=tMatches)
  matchFrames
}

mapContestToMeanGoals <- function(matchesRow, meanGoalsMap) {
  meanGoals <- vector(mode="double", 2)
  contest <- getGeneralContest(matchesRow[["Contest"]])
  
  if (contest == "qualifier") {
    meanGoals[1] <- meanGoalsMap[["qualifierHome"]]
    meanGoals[2] <- meanGoalsMap[["qualifierAway"]]
  }
  else if (contest == "group") {
    meanGoals[1] <- meanGoalsMap[["group"]]
    meanGoals[2] <- meanGoals[1]
  }
  else {
    meanGoals[1] <- meanGoalsMap[["knockout"]]
    meanGoals[2] <- meanGoals[1]
  }
  
  meanGoals
}
