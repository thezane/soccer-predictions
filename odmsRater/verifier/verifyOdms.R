verifyOdms <- function (currentYear, currentContest, location) {
  verifyModelSetup()
  dataPathIn <- "../../data/"
  fileName <- "matches.csv"
  matchSrc <- paste(dataPathIn, fileName, sep ="")
  matches <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matches <- getRelevantMatches(matches, currentContest, currentYear)
  matches <- addPredictions(matches, location, dataPathIn)
  dataPathOut <- "../../accuracy/"
  matchDest <- paste(dataPathOut, "verifiedOdms", currentYear, ".csv",
      sep="")
  write.csv(matches, matchDest, row.names=FALSE)
  verifyModel("Odms", currentYear)
}

verifyModelSetup <- function() {
  library(hash)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  lapply(srcFiles, source)
}

getRelevantMatches <- function (matches, currentContest, currentYear) {
  matchDateFormat <- "%m/%d/%y"
  matchYearFormat <- "%Y"
  matchDates <- format(as.Date(matches[["Date"]],
      format=matchDateFormat), format=matchYearFormat)
  matches <- matches[matchDates == currentYear, ]
  matches <- matches[rev(order(as.Date(matches[, "Date"],
      format=matchDateFormat))), ]
  areRelevantContests <- sapply(matches["Contest"],
      function(contest, currentContest.=currentContest)
      {grepl(currentContest, contest)})
  matches <- matches[areRelevantContests, ]
  matches
}

addPredictions <- function (matches, location, dataPath) {
  vNames <- c(
      "HomeStrAgg", "AwayStrAgg",
      "HomeAttack", "HomeDefense",
      "AwayAttack", "AwayDefense",
      "HomeWin", "Tie", "AwayWin")
  matches[vNames] <- 0
  header = "odms-"
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
    
    gamePrediction <- addPrediction(rData, matches[i, ], location)
    strNorm <- gamePrediction[["strNorm"]]
    matches[i, c("HomeStrAgg", "AwayStrAgg")] <-
        gamePrediction[["strAgg"]]
    matches[i, c("HomeAttack", "HomeDefense")] <- strNorm[1, ]
    matches[i, c("AwayAttack", "AwayDefense")] <- strNorm[2, ]
    matches[i, c("HomeWin", "Tie", "AwayWin")] <-
        gamePrediction[["gamePs"]]
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
  homeTeamName <- matchesRow[["HomeTeam"]]
  awayTeamName <- matchesRow[["AwayTeam"]]
  contestType <- getContestType(matchesRow[["Contest"]])
  gameHypo <- newGameHypo(homeTeamName, awayTeamName, contestType,
      location, rData)
  gamePrediction <- forecastGame(gameHypo)
  gamePrediction
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
