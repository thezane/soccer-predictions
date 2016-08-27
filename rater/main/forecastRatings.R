forecastRatings <- function(currentDate, contest) {
  library(hash)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  lapply(srcFiles, source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  readsData <- readData(currentDate, contest, dateFormat, dataPath)
  tTree <- readsData[["tTree"]]
  fTree <- readsData[["fTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  hA <- readsData[["hA"]]
  gi <- newGameIterator(gTree)
  gamesData <- normalizeGameGoals(gTree, gi, hA)
  rData <- optimizeRatings(tTree, fTree, gTree, gi, currentDate,
      contest)
  writeData(rData, T, dataPath)
  rData
}
