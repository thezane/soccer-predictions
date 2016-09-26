constructOptPrereqs <- function(gTree, gi, tTree, T) {
  meanGoalsMap <- constructMeanGoalsMap(T)
  gi <- reset(gi)
  goalsRelevant <- 0
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- normalizeGoals(game, meanGoalsMap)
    gTree[game$gameDateStr] <- gDateList
    goalsRelevant <- goalsRelevant +
        as.numeric(game$isRelevant) * sum(game$goals)
  }
  
  optPrereqs <- list(gTree=gTree, gi=gi, goalsRelevant=goalsRelevant,
      meanGoalsMap=meanGoalsMap)
  optPrereqs
}
