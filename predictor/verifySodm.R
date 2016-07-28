verifySodm <- function (currentYear, locations) {
  verifyModelSetup()
  dataPathIn <- "../data/"
  fileName <- "matches.csv"
  matchSrc <- paste(dataPathIn, fileName, sep ="")
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matches <- getRelevantMatches(matches, currentYear)
  matches <- addPredictions(matches, locations, dataPathIn)
  dataPathOut <- "../accuracy/"
  matchDest <- paste(dataPathOut, "verifiedSodm", currentYear, ".csv",
      sep="")
  write.csv(matches, matchDest, row.names=FALSE)
  verifyModel("Sodm", currentYear)
}

verifyModelSetup <- function() {
  library(hash)
  source("fitGoals.R")
  source("getGeneralContest.R")
  source("verifyModel.R")
}

getRelevantMatches <- function (matches, currentYear) {
  matchDateFormat <- "%m/%d/%y"
  matchYearFormat <- "%Y"
  matchDates <- format(as.Date(matches[["Date"]],
      format=matchDateFormat), format=matchYearFormat)
  matches <- matches[matchDates == currentYear, ]
  matches <- matches[rev(order(as.Date(matches[, "Date"],
      format=matchDateFormat))), ]
  matches
}

addPredictions <- function (matches, locations, dataPath) {
  vNames <- c("HomeWin", "Tie", "AwayWin")
  matches[vNames] <- 0
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
    
    matchPs <- addPrediction(forecastPrereq, matches[i, ], locations)
    matches[i, vNames] <- matchPs
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

addPrediction <- function(forecastPrereq, matchesRow, locations) {
  numDecimals <- 4
  homeTeam <- matchesRow[["HomeTeam"]]
  awayTeam <- matchesRow[["AwayTeam"]]
  contest <- getGeneralContest(matchesRow[["Contest"]])
  homeGoals <- matchesRow[["HomeGoals"]]
  awayGoals <- matchesRow[["AwayGoals"]]
  matchPrediction <- forecastMatch(homeTeam, awayTeam, contest,
      forecastPrereq, locations)
  matchPs <- matchPrediction[["MatchPs"]]
}
