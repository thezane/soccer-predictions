forecastRatings <- function(currentDate, currentContest="WOC") {
  library(hash)
  library(MASS)
  regexRFiles <- ".*\\.R"
  srcFiles <- list.files("../", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  utilitiesFiles <- list.files("../../utilities", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(c(srcFiles, utilitiesFiles), source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  rData <- readData(dateFormat, currentDate, dataPath)
  rData <- computeRatings(rData, currentContest)
  writeData(rData, dataPath)
  rData
}

