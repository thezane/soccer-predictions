fitGoals <- function (currentDate, matchSrc="", includeQs=FALSE) {
  fitGoalsSetup()
  matchSrc <- getMatchSrc(currentDate, matchSrc)  
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matchFrame <- computeDataFrame(matches, includeQs)
  matches <- matchFrame[["Matches"]]  
  meanGoalsMap <- matchFrame[["MeanGoalsMap"]]
  form1 <- ~c(HomeMeanGoals, AwayMeanGoals) +
      c(HomeAttack, AwayAttack) + c(AwayDefense, HomeDefense)
  model <- lm.dibp(HomeGoals~1, AwayGoals~1, l1l2=form1,
      common.intercept=TRUE, data=matches, pres=1e-08,
      distribution="geometric")
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

getMatchSrc <- function(currentDate, matchSrc) {
  if (matchSrc == "") {
    dataPath <- "../data/"
    header = "sodm-"
    fileType <- ".csv"
    matchSrc <- paste(dataPath, header, currentDate, fileType, sep="")
  }
  
  matchSrc
}
