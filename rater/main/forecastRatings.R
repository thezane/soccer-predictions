forecastRatings <- function(currentDate, currentContest) {
  library(hash)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  lapply(srcFiles, source)
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  readsData <- readData(currentDate, currentContest,
      dateFormat, dataPath)
  tTree <- readsData[["tTree"]]
  fTree <- readsData[["fTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  hA <- readsData[["hA"]]
  gi <- newGameIterator(gTree)
  gamesData <- normalizeGameGoals(gTree, gi, hA)
  gTree <- gamesData[["gTree"]]
  gi <- gamesData[["gi"]]
  hA <- gamesData[["hA"]]
  relevantGoals <- gamesData[["goalsRelevant"]]
  rData <- optimizeRatings(tTree, fTree, gTree, gi, hA, relevantGoals,
      currentDate)
  writeData(rData, T, dataPath)
  rData
}
