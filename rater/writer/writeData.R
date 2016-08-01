writeData <- function(gi, T, dataPath) {
  numDecimals <- 4
  colNames = c("HomeAttack", "HomeDefense",
      "AwayAttack", "AwayDefense",
      "HomeAttackNext", "HomeDefenseNext",
      "AwayAttackNext", "AwayDefenseNext")
  T[colNames] <- 0
  gi <- reset(gi)
  endDate <- ""
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    T <- updateMatches(T, game, numDecimals)
    endDate <- game$gameDate
  }
  
  T <- T[order(as.Date(matches[, "Date"]), decreasing=TRUE), ]
  endDate <- gsub("/", "-", endDate)
  outFile <- paste(dataPath, "sodm-", endDate, sep="")
  write.csv(T, outFile, row.names=FALSE)
}

updateGames <- function(T, game, numDecimals) {
  i <- game$gameRow
  strNorm <- round(computeStrNorm(game$teamStr), numDecimals);
  strNextNorm <- round(computeStrNorm(game$teamStrNext), numDecimals)
  T[i, 'HomeAttack'] = strNorm[1, 1];
  T[i, 'HomeDefense'] = strNorm[1, 2];
  T[i, 'AwayAttack'] = strNorm[2, 1];
  T[i, 'AwayDefense'] = strNorm[2, 2];
  T[i, 'HomeAttackNext'] = strNormNext[1, 1];
  T[i, 'HomeDefenseNext'] = strNormNext[1, 2];
  T[i, 'AwayAttackNext'] = strNormNext[2, 1];
  T[i, 'AwayDefenseNext'] = strNormNext[2, 2];
  T 
}
