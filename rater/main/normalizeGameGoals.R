normalizeGameGoals <- function(gTree, gi, hA) {
  hAData <- computeHA(hA)
  gi <- reset(gi)
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- normalizeGoals(game, hAData)
    gTree[game$gameDateStr] <- gDateList
  }
  
  gamesData <- list(gTree=gTree, gi=gi)
  gamesData
}
