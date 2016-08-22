normalizeGameGoals <- function(gTree, gi, hA) {
  hAData <- computeHA(hA)
  qHA <- hAData[["qHA"]]
  tHA <- hAData[["tHA"]]
  gi <- reset(gi)
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    game$goalsNorm <- computeGoalsNorm(game, qHA, tHA)
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- game
    gTree[game$gameDateStr] <- gDateList
  }
  
  gamesData <- list(gTree=gTree, gi=gi)
  gamesData
}

computeGoalsNorm <- function(game, qHA, tHA) {
  goals <- game$goals
  
  if (game$isQualifier) {
    goals[1] <- goals[1] / qHA
  }
  else if (game$existsHA) {
    goals[1] <- goals[1] / tHA
  }

  goals
}
