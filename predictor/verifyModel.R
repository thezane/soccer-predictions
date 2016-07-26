verifyModel <- function (currentYear) {
  verifyModelSetup()
  dataPath <- "../data/"
  fileName <- "matches.csv"
  matchSrc <- paste(dataPath, fileName, sep ="")
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matches <- getRelevantMatches(matches, currentYear)
  matches <- addMeanSquaredErrors(matches, dataPath)
  matches
}

verifyModelSetup <- function() {
  library(hash)
  source("fitGoals.R")
  source("getGeneralContest.R")
}

getRelevantMatches <- function (matches, currentYear) {
  matchDateFormat <- "%m/%d/%y"
  matchYearFormat <- "%Y"
  matchDates <- format(as.Date(matches[["Date"]],
      format=matchDateFormat), format=matchYearFormat)
  matches[matchDates == currentYear, ]
}

addMeanSquaredErrors <- function (matches, dataPath) {
  matches["MSE"] <- 0
  header = "sodm-"
  fileType <- ".csv"
  matchSrc <- ""
  n <- nrow(matches)
  i <- 1
  
  while (i <= n) {
    currentDate <- as.Date(formatDate(matches[i, "Date"])) - 1
    matchSrcNext <- getNextMatchSrc(currentDate, 
        dataPath, header, fileType)
    
    if (matchSrcNext != matchSrc) {
      matchSrc <- matchSrcNext
      forecastPrereq <- fitGoals("", matchSrc)
    }
    
    mse <- computeMeanSquaredError(forecastPrereq, matches[i, ])
    matches[i, "MSE"] <- mse
    i <- i + 1
  }
  
  matches
}

formatDate <- function(oldDate) {
  oldDateFormat <- "%m/%d/%y"
  newDateFormat <- "%Y-%m-%d"
  newDate <- format(as.Date(oldDate,
      format=oldDateFormat), format=newDateFormat)
}

getNextMatchSrc <- function(currentDate, dataPath, header, fileType) {
  while (TRUE) {
    matchSrc <- paste(dataPath, header, currentDate, fileType, sep="")
    
    if (file.exists(matchSrc)) {
      break
    }
    else {
      currentDate <- currentDate - 1
    }
  }
  
  matchSrc
}

computeMeanSquaredError <- function(forecastPrereq, matchesRow) {
  homeTeam <- matchesRow[["HomeTeam"]]
  awayTeam <- matchesRow[["AwayTeam"]]
  contest <- getGeneralContest(matchesRow[["Contest"]])
  homeGoals <- matchesRow[["HomeGoals"]]
  awayGoals <- matchesRow[["AwayGoals"]]
  matchPrediction <- forecastMatch(homeTeam, awayTeam, contest,
      forecastPrereq)
  matchPs <- matchPrediction[["MatchPs"]]
  matchResults <- c(homeGoals > awayGoals, homeGoals == awayGoals,
      homeGoals < awayGoals)
  mse <- sum((unlist(matchPs) - matchResults) ^ 2)
  mse
}
