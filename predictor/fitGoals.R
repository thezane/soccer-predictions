fitGoals <- function (currentDate, matchSrc="") {
  fitGoalsSetup()
  
  if (matchSrc == "") {
    dataPath <- "../data/"
    header = "sodm-"
    fileType <- ".csv"
    matchSrc <- paste(dataPath, header, currentDate, fileType, sep="")
  }
  
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matchFrames <- computeDataFrame(matches)
  matches <- matchFrames[["Matches"]]
  tMatches <- matchFrames[["TMatches"]]
  meanGoalsMap <- matchFrames[["MeanGoalsMap"]]
  form1 <- ~c(HomeMeanGoals, AwayMeanGoals) +
      c(HomeAttack, AwayAttack) + c(AwayDefense, HomeDefense)
  model <- lm.bp(HomeGoals~1, AwayGoals~1, l1l2=form1,
      common.intercept=TRUE, data=tMatches, pres=1e-06)
  teams <- constructTeams(matches)
  forecastPrereq <- list("meanGoalsMap"=meanGoalsMap,
      "matches"=matches, "model"=model, "teams"=teams)
  forecastPrereq
}

fitGoalsSetup <- function() {
  library(hash)
  source("computeDataFrame.R")
  source("constructTeams.R")
  source("forecastMatch.R")
  source("getGeneralContest.R")
  bivPoisFiles <- list.files("bivpois-Rcode", full.names=TRUE)
  sapply(bivPoisFiles, source)
}
