writeIter <- function(y, x, iterFile) {
  iterData <- matrix(c(y, x), nrow=1)
  write.table(iterData, iterFile, append=TRUE,
      col.names=FALSE, row.names=FALSE, sep=",")
}
