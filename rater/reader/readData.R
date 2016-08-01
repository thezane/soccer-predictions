readData <- function(currentDate, dateFormat, dataPath) {
  teamsData <- constructTeams(dataPath)
  tTree <- teamsData[["tTree"]]
  fTree <- teamsData[["fTree"]]
  gamesData <- constructGames(currentDate, dateFormat, tTree, dataPath)
  gTree <- gamesData[["gTree"]]
  T <- gamesData[["T"]]
  hA <- gamesData[["hA"]]
  readsData <- list(tTree=tTree, fTree=fTree, gTree=gTree, T=T, hA=hA)
  readsData
} 
