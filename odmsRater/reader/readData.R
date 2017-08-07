readData <- function(currentDate, dateFormat, dataPath) {
  teamsData <- constructTeams(dataPath)
  tTree <- teamsData[["tTree"]]
  fTree <- teamsData[["fTree"]]
  gTree <- constructTeamChanges(dateFormat, dataPath)
  gamesData <- constructGames(gTree, currentDate, dateFormat, dataPath)
  gTree <- gamesData[["gTree"]]
  T <- gamesData[["T"]]
  readsData <- list(tTree=tTree, fTree=fTree, gTree=gTree, T=T)
  readsData
} 
