writeGames <- function(rData, T, dataPath) {
  rOutput <- rData[["rOutput"]]
  T <- getGamesData(rOutput$gi, T)
  outFile <- paste(dataPath, "odms-matches", sep="")
  save(rData, file=paste(outFile, ".RData", sep=""))
  write.csv(T, paste(outFile, ".csv", sep=""), row.names=FALSE)
}

getGamesData <- function(gi, T) {
  colNames = c(
      "HomeStrAgg", "AwayStrAgg",
      "HomeStrAggNext", "AwayStrAggNext",
      "HomeAttack", "HomeDefense",
      "AwayAttack", "AwayDefense",
      "HomeAttackNext", "HomeDefenseNext",
      "AwayAttackNext", "AwayDefenseNext",
      "HomeWin", "Tie", "AwayWin",
      "SSE")
  T[colNames] <- 0
  gi <- reset(gi)
  endDate <- "" 

  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    T <- updateGames(T, game)
    endDate <- game$gameDate
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
  T[i, "Type"] <- game$type
  T[i, "HomeStrAgg"] <- strAgg[1]
  T[i, "AwayStrAgg"] <- strAgg[2]
  T[i, "HomeStrAggNext"] <- strAggNext[1]
  T[i, "AwayStrAggNext"] <- strAggNext[2]
  T[i, "HomeAttack"] <- strNormBeta[1, 1]
  T[i, "HomeDefense"] <- strNormBeta[1, 2]
  T[i, "AwayAttack"] <- strNormBeta[2, 1]
  T[i, "AwayDefense"] <- strNormBeta[2, 2]
  T[i, "HomeAttackNext"] <- strNextNormBeta[1, 1]
  T[i, "HomeDefenseNext"] <- strNextNormBeta[1, 2]
  T[i, "AwayAttackNext"] <- strNextNormBeta[2, 1]
  T[i, "AwayDefenseNext"] <- strNextNormBeta[2, 2]
  T[i, "HomeWin"] <- ps[1]
  T[i, "Tie"] <- ps[2]
  T[i, "AwayWin"] <- ps[3]
  T[i, "SSE"] <- round(game$sse, numDecimals)
  T 
}
