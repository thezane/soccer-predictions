writeIter <- function(y, x, dataPath) {
  iterData <- matrix(c(y, x), nrow=1)
  outFile <- paste(dataPath, "odms-iters", endDate, sep="")
  write.table(iterData, paste(outFile, ".csv", sep=""), append=TRUE,
      row.names=FALSE)
}
