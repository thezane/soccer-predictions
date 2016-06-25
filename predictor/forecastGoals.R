#Load libs and src files.
library(hash)
source("computeDataFrame.R")
source("computeMeanGoals.R")
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
form <- ~c(HomeMeanGoals, AwayMeanGoals) + c(HomeAttack, AwayAttack) +
    c(AwayDefense, HomeDefense)
model <- lm.dibp(HomeGoals~1, AwayGoals~1, l1l2=form,
    common.intercept=TRUE, data=matches, distribution="geometric")
