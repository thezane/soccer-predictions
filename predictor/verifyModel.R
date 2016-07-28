verifyModel <- function (currentYear, locations) {
  verifyModelSetup()
  dataPath <- "../data/"
  fileName <- "matches.csv"
  matchSrc <- paste(dataPath, fileName, sep ="")
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matches <- getRelevantMatches(matches, currentYear)
  matches <- addMSEs(matches, locations, dataPath)
  matchDest <- paste(dataPath, "verified-", currentYear, ".csv",
      sep="")
  write.csv(matches, matchDest, row.names=FALSE)
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
  matches <- matches[rev(order(as.Date(matches[, "Date"],
      format=matchDateFormat))), ]
}

addMSEs <- function (matches, locations, dataPath) {
  vNames <- c("HomeWin", "Tie", "AwayWin", "MSE")
  matches[vNames] <- 0
  header = "sodm-"
  fileType <- ".csv"
  matchSrc <- ""
  n <- nrow(matches)
  i <- 1
  
  while (i <= n) {
    currentDate <- as.Date(formatDate(matches[i, "Date"])) - 1
    print(currentDate)
    matchSrcNext <- getNextMatchSrc(currentDate, 
        dataPath, header, fileType)
    
    if (matchSrcNext != matchSrc) {
      matchSrc <- matchSrcNext
      forecastPrereq <- fitGoals("", matchSrc)
    }
    
    matchV <- computeV(forecastPrereq, matches[i, ], locations)
    matches[i, vNames] <- matchV
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

computeV <- function(forecastPrereq, matchesRow, locations) {
  numDecimals <- 4
  homeTeam <- matchesRow[["HomeTeam"]]
  awayTeam <- matchesRow[["AwayTeam"]]
  contest <- getGeneralContest(matchesRow[["Contest"]])
  homeGoals <- matchesRow[["HomeGoals"]]
  awayGoals <- matchesRow[["AwayGoals"]]
  matchPrediction <- forecastMatch(homeTeam, awayTeam, contest,
      forecastPrereq, locations)
  matchPs <- matchPrediction[["MatchPs"]]
  matchResults <- c(homeGoals > awayGoals, homeGoals == awayGoals,
      homeGoals < awayGoals)
  mse <- round(sum((unlist(matchPs) - matchResults) ^ 2), numDecimals)
  matchV <-append(matchPs, list("MSE"=mse))
  matchV
}
