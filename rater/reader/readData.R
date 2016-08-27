readData <- function(currentDate, contest, dateFormat, dataPath) {
  teamsData <- constructTeams(dataPath)
  tTree <- teamsData[["tTree"]]
  fTree <- teamsData[["fTree"]]
  gamesData <- constructGames(currentDate, tTree, contest, dateFormat,
      dataPath)
  gTree <- gamesData[["gTree"]]
  T <- gamesData[["T"]]
  hA <- gamesData[["hA"]]
  readsData <- list(tTree=tTree, fTree=fTree, gTree=gTree, T=T, hA=hA)
  readsData
} 
