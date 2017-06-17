readIter <- function(iterFile) {
  iterData <- read.csv(iterFile, header=FALSE)
  minCostI <- nrow(iterData)
  x <- as.numeric(iterData[minCostI, -1])
  x
}
