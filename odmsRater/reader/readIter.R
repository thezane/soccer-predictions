readIter <- function(iterFile) {
  iterData <- read.csv(iterFile)
  minCostI <- which.min(iterData[[1]])
  x <- as.numeric(iterData[minCostI, -1])
  x
}
