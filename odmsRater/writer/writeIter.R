writeIter <- function(y, x, dataPath) {
  iterData <- matrix(c(y, x), nrow=1)
  outFile <- paste(dataPath, "odms-iters", sep="")
  write.table(iterData, paste(outFile, ".csv", sep=""), append=TRUE,
      col.names=FALSE, row.names=FALSE)
}
