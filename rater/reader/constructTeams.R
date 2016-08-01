constructTeams <- function(dataPath) {
  tTree <- hash()
  fTree <- hash()
  teamsSrc <- paste(dataPath, "teams.csv", sep="")
  T <- read.csv(teamsSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- ncol(T)
  i <- 1
  
  while (i <= n) {
    teamData <- addTeam(T, i, tTree)
    tTree <- teamData[["tTree"]]
    fName <- teamData[["fName"]]
    fTree <- addFederation(fTree, fName)
    i <- i + 1
  }
  
  teamsData <- list(tTree=tTree, fTree=fTree)
  teamsData
}

addTeam <- function(T, i, tTree) {
  teamName <- T[[i, "Team"]]
  fName <- T[[i, "Federation"]]
  tTree[teamName] <- newTeam(teamName, fName)
  teamData <- list(tTree=tTree, fName=fName)
  teamData
}

addFederation <- function(fTree, fName) {
  if (!has.key(fName, fTree)) {
    fTree[[fName]] <- c(1, 1)
  }
  
  fTree
}
