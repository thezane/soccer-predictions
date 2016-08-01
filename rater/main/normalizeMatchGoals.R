normalizeMatchGoals <- function(gTree, gi, hA) {
  hAData <- computeHA(hA)
  qHA <- hAData[["qHA"]]
  tHA <- hAData[["tHA"]]
  gi <- reset(gi)
  
  while (hasNextGame(hA)) {
    gameData <- nextGame(gi)
    
  }
}
