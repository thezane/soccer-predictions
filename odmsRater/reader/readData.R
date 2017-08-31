readData <- function(rOptions, inputPath) {
  tTree <- constructTeams(inputPath)
  gTree <- constructChanges(rOptions, inputPath)
  gamesData <- constructGames(gTree, rOptions, inputPath)
  gTree <- gamesData[["gTree"]]
  T <- gamesData[["T"]]
  readsData <- list(tTree=tTree, gTree=gTree, T=T)
  readsData
} 
