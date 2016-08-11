forecastRatings <- function(currentDate) {
  library(hash)
  library(neldermead)
  srcFiles <- list.files("../", ".*\\.R",
      full.names=TRUE, recursive=TRUE)
  lapply(srcFiles, source)
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
  rOutput <- optimizeRatings(tTree, fTree, gTree, gi)
  gi <- rOutput$gi
  writeData(gi, T, dataPath)
  rOutput
}
