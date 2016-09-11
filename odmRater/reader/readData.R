readData <- function(dateFormat, currentDate, dataPath) {
  matchSrc <- paste(dataPath, "matches.csv", sep="")
  T <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  games <- newGames(T, currentDate, dateFormat)
  tTree <- constructTeams(dataPath)
  readsData <- list(games=games, tTree=tTree)
  readsData
}

constructTeams <- function(dataPath) {
  tTree <- hash()
  teamsSrc <- paste(dataPath, "teams.csv", sep="")
  T <- read.csv(teamsSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- nrow(T)
  i <- 1
  
  while (i <= n) {
    tTree <- addTeam(T, i, tTree)
    i <- i + 1
  }
  
  tTree
}

addTeam <- function(T, i, tTree) {
  teamName <- T[[i, "Team"]]
  tTree[teamName] <- newTeam(teamName, i)
  tTree
}
