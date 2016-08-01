forecastRatings <- function(currentDate) {
  forecastRatingsSetup()
  dateFormat <- "%m/%d/%y"
  dataPath <- "../../data/"
  currentDate <- as.Date(currentDate, dateFormat)
  readsData <- readData(currentDate, dateFormat, dataPath)
  tTree <- readsData[["tTree"]]
  fTree <- readsData[["fTree"]]
  gTree <- readsData[["gTree"]]
  T <- readsData[["T"]]
  hA <- readsData[["hA"]]
  gi <- newGameIterator(gTree)
  gamesData <- normalizeGameGoals(gTree, gi, hA)
  rOutput <- optimizeRatings(tTree, fTree, gTree, gi, nrow(T))
  gi <- rOutput$gi
  writeData(gi, T, dataPath)
  rOutput
}

forecastRatingsSetup <- function() {
  library(hash)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  lapply(srcFiles, source)
}
