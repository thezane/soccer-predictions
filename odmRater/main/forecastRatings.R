forecastRatings <- function(currentDate, currentContest="WOC") {
  library(hash)
  library(MASS)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  rData <- readData(dateFormat, currentDate, dataPath)
  rData <- computeRatings(rData, currentContest)
  writeData(rData, dataPath)
  rData
}

