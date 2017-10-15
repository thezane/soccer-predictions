writeGames <- function(rData, T, outputPath) {
  rOptions <- rData[["rOptions"]]
  rOutput <- rData[["rOutput"]]
  T <- getGamesData(rOutput$gi, T)
  outFile <- paste(outputPath, rOptions$writeName, ".csv", sep="")
  outFileR <- paste(outputPath, rOptions$writeName, ".RData", sep="")
  write.csv(T, outFile, row.names=FALSE)
  save(rData, file=outFileR)
}

getGamesData <- function(gi, T) {
  colNames = c(
      "HomeStrAgg", "AwayStrAgg",
      "HomeStrAggNext", "AwayStrAggNext",
      "HomeWin", "Tie", "AwayWin",
      "SSE")
  T[colNames] <- 0
  gi <- reset.EventIterator(gi)

  while (hasNext.EventIterator(gi)) {
    eventData <- next.EventIterator(gi)
    gi <- eventData[["gi"]]
    event <- eventData[["event"]]
    
    if (class(event) == "Game") {
	  game <- event
	  T <- updateGames(T, game)	
    }
  }
  
  T <- T[order(as.Date(T[, "Date"]), decreasing=TRUE), ]
  T
}

updateGames <- function(T, game) {
  numDecimals <- 4
  i <- game$gameRow
  strAgg <- round(game$strAgg, numDecimals)
  strAggNext <- round(game$strAggNext, numDecimals)
  strNormBeta <- round(game$strNormBeta, numDecimals)
  strNextNormBeta <- round(game$strNextNormBeta, numDecimals)
  ps <- round(game$Ps, numDecimals)
  T[i, "Date"] <- game$gameDateStr
  T[i, "Contest"] <- game$contest
  T[i, "HomeStrAgg"] <- strAgg[1]
  T[i, "AwayStrAgg"] <- strAgg[2]
  T[i, "HomeStrAggNext"] <- strAggNext[1]
  T[i, "AwayStrAggNext"] <- strAggNext[2]
  T[i, "HomeWin"] <- ps[1]
  T[i, "Tie"] <- ps[2]
  T[i, "AwayWin"] <- ps[3]
  T[i, "SSE"] <- round(game$sse, numDecimals)
  T 
}
