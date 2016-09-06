verifySodm <- function (currentYear, location) {
  verifyModelSetup()
  dataPathIn <- "../../data/"
  fileName <- "matches.csv"
  matchSrc <- paste(dataPathIn, fileName, sep ="")
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matches <- getRelevantMatches(matches, currentYear)
  matches <- addPredictions(matches, location, dataPathIn)
  dataPathOut <- "../../accuracy/"
  matchDest <- paste(dataPathOut, "verifiedSodm", currentYear, ".csv",
      sep="")
  write.csv(matches, matchDest, row.names=FALSE)
  verifyModel("Sodm", currentYear)
}

verifyModelSetup <- function() {
  library(hash)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  lapply(srcFiles, source)
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

addPredictions <- function (matches, location, dataPath) {
  vNames <- c("HomeWin", "Tie", "AwayWin")
  matches[vNames] <- 0
  header = "sodm-"
  fileType <- ".RData"
  matchSrc <- ""
  n <- nrow(matches)
  i <- 1
  
  while (i <= n) {
    currentDate <- as.Date(formatDate(matches[i, "Date"])) - 1
    matchSrcNext <- getNextMatchSrc(currentDate, 
        dataPath, header, fileType)
    
    if (matchSrcNext != matchSrc) {
      matchSrc <- matchSrcNext
      load(matchSrc)
    }
    
    gamePs <- addPrediction(rData, matches[i, ], location)
    matches[i, vNames] <- gamePs
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

addPrediction <- function(rData, matchesRow, location) {
  numDecimals <- 4
  homeTeamName <- matchesRow[["HomeTeam"]]
  awayTeamName <- matchesRow[["AwayTeam"]]
  contestType <- getContestType(matchesRow[["Contest"]])
  gameHypo <- newGameHypo(homeTeamName, awayTeamName, contestType,
      location, rData)
  gamePrediction <- forecastGame(gameHypo)
  gamePs <- round(gamePrediction[["gamePs"]], numDecimals)
  gamePs
}

getContestType <- function(contest) {
  if (grepl("-Q", contest)) {
    contestType <- "-Q"
  }
  else {
    contestType <- "-T"
  }
  
  contestType
} 
