dataPath <- "../data/"
matchFile <- "ratedMatches.csv"
matchSrc <- paste(dataPath, matchFile, sep ="")
matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"")
