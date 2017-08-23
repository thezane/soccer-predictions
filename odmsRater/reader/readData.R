readData <- function(rOptions, dataPath) {
  tTree <- constructTeams(dataPath)
  gTree <- constructChanges(rOptions, dataPath)
  gamesData <- constructGames(gTree, rOptions, dataPath)
  gTree <- gamesData[["gTree"]]
  T <- gamesData[["T"]]
  readsData <- list(tTree=tTree, gTree=gTree, T=T)
  readsData
} 
