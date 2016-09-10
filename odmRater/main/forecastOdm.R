forecastOdm <- function(currentDate, currentContest="WOC") {
  library(hash)
  library(MASS)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  rOutput <- readData(currentDate, dateFormat, dataPath)
  rOutput <- computeRatings(rOutput)
  rOutput
}

