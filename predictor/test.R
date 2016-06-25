library(hash)
source("computeMeanGoals.R")

dataPath <- "../data/"
matchFile <- "ratedMatches.csv"
matchSrc <- paste(dataPath, matchFile, sep ="")
matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
    stringsAsFactors=FALSE)
meanGoals <- computeMeanGoals(matches)
