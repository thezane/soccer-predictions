readData <- function(currentDate, dateFormat, dataPath) {
  teamsData <- constructTeams(dataPath)
  tTree <- teamsData[["tTree"]]
  fTree <- teamsData[["fTree"]]
  gamesData <- constructGames(currentDate, tTree, dateFormat, dataPath)
  gTree <- gamesData[["gTree"]]
  T <- gamesData[["T"]]
  readsData <- list(tTree=tTree, fTree=fTree, gTree=gTree, T=T)
  readsData
} 
