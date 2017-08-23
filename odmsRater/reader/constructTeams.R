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
  fName <- T[[i, "Federation"]]
  tTree[teamName] <- newTeam(teamName, fName)
  tTree
}
