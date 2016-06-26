#Load libs and src files.
library(hash)
source("computeDataFrame.R")
source("computeMeanGoals.R")
source("constructTeams.R")
source("verifyModel.R")
bivPoisFiles <- list.files("bivpois-Rcode", full.names=TRUE)
sapply(bivPoisFiles, source)

#Prepare data frame.
dataPath <- "../data/"
matchFile <- "ratedMatches.csv"
matchSrc <- paste(dataPath, matchFile, sep ="")
matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
    stringsAsFactors=FALSE)
meanGoalsMap <- computeMeanGoals(matches)
matches <-computeDataFrame(matches, meanGoalsMap)

#Fit model.
form1 <- ~c(HomeMeanGoals, AwayMeanGoals) + c(HomeAttack, AwayAttack) +
    c(AwayDefense, HomeDefense)
model <- lm.bp(HomeGoals~1, AwayGoals~1, l1l2=form1,
    common.intercept=TRUE, data=matches, pres=1e-06)

#Add teams.
teams <- constructTeams(matches)
