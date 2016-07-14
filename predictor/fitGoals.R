fitGoals <- function (currentDate) {
  source("setup.R")
  dataPath <- "../data/"
  header = "sodm-"
  fileType <- ".csv"
  matchSrc <- paste(dataPath, header, currentDate, fileType, sep ="")
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  meanGoalsMap <- computeMeanGoals(matches)
  matches <-computeDataFrame(matches, meanGoalsMap)
  form1 <- ~c(HomeMeanGoals, AwayMeanGoals) +
      c(HomeAttack, AwayAttack) + c(AwayDefense, HomeDefense)
  model <- lm.bp(HomeGoals~1, AwayGoals~1, l1l2=form1,
      common.intercept=TRUE, data=matches, pres=1e-06)
  teams <- constructTeams(matches)
  forecastPrereq <- list("meanGoalsMap"=meanGoalsMap,
      "matches"=matches, "model"=model, "teams"=teams)
  forecastPrereq
}
