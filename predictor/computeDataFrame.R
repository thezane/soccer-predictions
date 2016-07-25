computeDataFrame <-function(matches) {
  generalContests <- unlist(lapply(matches[, "Contest"],
      getGeneralContest))
  matches[, "GeneralContest"] <- generalContests
  meanGoalsMap <- computeMeanGoals(matches)
  meanGoals <- apply(matches, 1, mapContestToMeanGoals,
      meanGoalsMap=meanGoalsMap)
  meanGoals <- t(meanGoals)
  matches[["HomeMeanGoals"]] <- meanGoals[, 1]
  matches[["AwayMeanGoals"]] <- meanGoals[, 2]
  matches <- matches[rev(order(as.Date(matches[, "Date"],
      format="%Y/%m/%d"))), ]
  matchContests <- lapply(matches[, "Contest"], getGeneralContest)
  tMatches <- matches[matchContests != "qualifier", ]
  rownames(tMatches) <- 1:nrow(tMatches)
  matchFrames <- list("Matches"=matches, "TMatches"=tMatches,
      "MeanGoalsMap"=meanGoalsMap)
  matchFrames
}

computeMeanGoals <- function (matches) {
  homeGoals <- aggregate(matches[, "HomeGoals"],
      list(matches$GeneralContest), mean)
  awayGoals <- aggregate(matches[, "AwayGoals"],
      list(matches$GeneralContest), mean)
  h <- hash()
  h["group"] <- mean(c(homeGoals[1, 2], awayGoals[1, 2]))
  h["knockout"] <- mean(c(homeGoals[2, 2], awayGoals[2, 2]))
  h["qualifierHome"] <- homeGoals[3, 2]
  h["qualifierAway"] <- awayGoals[3, 2]
  h
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
