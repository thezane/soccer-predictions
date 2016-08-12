computeDataFrame <-function(matches) {
  teams <- constructTeams(matches)
  generalContests <- unlist(lapply(matches[, "Contest"],
      getGeneralContest))
  matches[, "GeneralContest"] <- generalContests
  meanGoalsMap <- computeMeanGoals(matches)
  matchContests <- lapply(matches[, "Contest"], getGeneralContest)
  matches <- matches[matchContests != "q", ]
  rownames(matches) <- 1:nrow(matches)
  meanGoals <- apply(matches, 1, mapContestToMeanGoals,
      meanGoalsMap=meanGoalsMap)
  meanGoals <- t(meanGoals)
  matches[["HomeMeanGoals"]] <- meanGoals[, 1]
  matches[["AwayMeanGoals"]] <- meanGoals[, 2]
  matches <- matches[rev(order(as.Date(matches[, "Date"],
      format="%Y/%m/%d"))), ]
  forecastPrereq <- list("Matches"=matches,
      "MeanGoalsMap"=meanGoalsMap, "Teams"=teams)
  forecastPrereq
}

computeMeanGoals <- function (matches) {
  matchGoals <- aggregateMatchGoals(matches, sum)
  numMatches <- aggregateMatchGoals(matches, length)
  meanGoalsMap <- hash()
  meanGoalsMap["qHome"] <- extractGoals(matchGoals, numMatches,
      "qHome")
  meanGoalsMap["qAway"] <- extractGoals(matchGoals, numMatches,
      "qAway")
  meanGoalsMap["tHome"] <- extractGoals(matchGoals, numMatches,
      "tHome")
  meanGoalsMap["tAway"] <- extractGoals(matchGoals, numMatches,
      "tAway")
  meanGoalsMap
}

aggregateMatchGoals <- function(matches, fun) {
  homeGoals <- aggregate(matches[, "HomeGoals"],
      list(matches$GeneralContest, matches$HomeAdvantage), fun)
  awayGoals <- aggregate(matches[, "AwayGoals"],
      list(matches$GeneralContest, matches$HomeAdvantage), fun)
  homeQGoals = homeGoals[which(homeGoals$Group.1 == "q"), ]
  awayQGoals = awayGoals[which(awayGoals$Group.1 == "q"), ]
  homeTGoals = homeGoals[intersect(
      which(homeGoals$Group.1 == "t"),
      which(homeGoals$Group.2 == TRUE)), ]
  awayTGoalsHA <- homeGoals[intersect(
      which(homeGoals$Group.1 == "t"),
      which(homeGoals$Group.2 == FALSE)), ]
  awayTGoalsNeutral <- awayGoals[
      which(awayGoals$Group.1 == "t"), ]
  awayTGoalsFrame <- rbind(awayTGoalsHA, awayTGoalsNeutral)
  awayTGoals <- colSums(awayTGoalsFrame["x"])
  matchGoals <- list(
      "qHome"=homeQGoals[["x"]], "qAway"=awayQGoals[["x"]],
      "tHome"=homeTGoals[["x"]], "tAway"=awayTGoals[["x"]])
  matchGoals
}

extractGoals <- function(matchGoals, numMatches, contest) {
  log(matchGoals[[contest]] / numMatches[[contest]])
}

mapContestToMeanGoals <- function(matchesRow, meanGoalsMap) {
  meanGoals <- vector(mode="double", 2)
  contest <- getGeneralContest(matchesRow[["Contest"]])
  existsHA <- as.numeric(matchesRow[["HomeAdvantage"]])
 
  if (contest == "q") {
    meanGoals[1] <- meanGoalsMap[["qHome"]]
    meanGoals[2] <- meanGoalsMap[["qAway"]]   
  }
  else if (existsHA) {
    meanGoals[1] <- meanGoalsMap[["tHome"]]
    meanGoals[2] <- meanGoalsMap[["tAway"]]
  }
  else {
    meanGoals[1] <- meanGoalsMap[["tAway"]]
    meanGoals[2] <- meanGoals[1]
  }
  
  meanGoals
}
