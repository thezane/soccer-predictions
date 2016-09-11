writeData <- function(rData, dataPath) {
  T <- updateT(rData[["games"]])
  endDate <- rData[["endDate"]]
  outFile <- paste(dataPath, "odm-", endDate, sep="")
  save(rData, file=paste(outFile, ".RData", sep=""))
  write.csv(T, paste(outFile, ".csv", sep=""), row.names=FALSE)
}

updateT <- function(games) {
  T <- games$T
  colNames <- games$colNames
  n <- length(colNames)
  numDecimals <- 4
  j <- 1

  while (j <= n) {
    colName <- colNames[j]
    T[, colName] <- round(T[, colName], numDecimals)
    j <- j + 1
  }

  T
}
