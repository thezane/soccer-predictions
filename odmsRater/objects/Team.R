newTeam <- function(teamName, fName, adopterName) {
  team <- list(
    name=teamName,
    fName=fName,
    adopterName=adopterName,
    strNorm=c(0, 0),
    strAgg=0,
    numUpdates=0,
    updateDate=as.Date("0001-01-01")
  )
  
  class(team) <- "Team"
  team
} 

resetTeam <- function(team, rOptions) {
  team$strNorm <- c(0, 0)
  team$strAgg <- 0
  team$numUpdates <- 0
  team$updateDate <- as.Date("0001-01-01")
  team
}

updateTeam <- function(team, game, i) {
  team$updateDate <- game$gameDate
  team$strNorm <- game$strNextNorm[i, ]
  team$strAgg <- game$strAggNext[i]
  team$numUpdates <- team$numUpdates + 1
  team
}

getTeamStrs <- function(team, rOptions, rOutput) {
  if (team$numUpdates > 0) {
    strNorm <-team$strNorm
    strAgg <- team$strAgg
  }
  else if (team$numUpdates == 0 && team$adopterName != "None") {
    tTree <- rOutput[["tTree"]]
    adopter <- tTree[[team$adopterName]]
    strNorm=adopter$strNorm
    strAgg <- adopter$strAgg
  }
  else {
	strNorm=rOptions$fTree[[team$fName]]
    strBetas <- rOptions$strBetas
    strAgg <- strNorm %*% strBetas
  }

  teamStrs <- list(strNorm=strNorm, strAgg=strAgg)
  teamStrs
}
