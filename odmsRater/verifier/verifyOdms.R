verifyOdms <- function (matchesSrc, currentYear, currentContest,
    location) {
  verifyModelSetup()
  matches <- read.csv(matchesSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  matches <- getRelevantMatches(matches, currentContest, currentYear)
  dataPathOut <- "../../accuracy/"
  matchDest <- paste(dataPathOut, "verifiedOdms", currentYear, ".csv",
      sep="")
  write.csv(matches, matchDest, row.names=FALSE)
  verifyModel("Odms", currentYear)
}

verifyModelSetup <- function() {
  library(hash)
  regexRFiles <- ".*\\.R"
  srcFiles <- list.files("../", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  utilitiesFiles <- list.files("../../utilities", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  lapply(c(srcFiles, utilitiesFiles), source)
}

getRelevantMatches <- function (matches, currentContest, currentYear) {
  matchDateFormat <- "%Y-%m-%d"
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
