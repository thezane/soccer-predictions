computeOptPrereqs <- function(gTree, gi, hA) {
  hA <- updateHAData(hA)
  gi <- reset(gi)
  goalsRelevant <- 0
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- normalizeGoals(game, hA$hAData)
    gTree[game$gameDateStr] <- gDateList
    goalsRelevant <- goalsRelevant +
        as.numeric(game$isRelevant) * sum(game$goals)
  }
  
  optPrereqs <- list(gTree=gTree, gi=gi, hA=hA,
      goalsRelevant=goalsRelevant)
  optPrereqs
}
