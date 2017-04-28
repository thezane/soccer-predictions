newTeam <- function(teamName, fName) {
  team <- list(
    name=teamName,
    fName=fName,
    teamStr=c(1, 1),
    strNorm=c(0, 0),
    strAgg=0,
    isUpdated=FALSE,
    updateDate=as.Date("0001-01-01")
  )
  
  class(team) <- "Team"
  team
} 

resetTeam <- function(team, rOptions) {
  team$teamStr <- c(1, 1)
  team$strNorm <- c(0, 0)
  team$strAgg <- 0
  team$isUpdated <- FALSE
  team$updateDate <- as.Date("0001-01-01")
  team$xp <- rOptions$xpDefault
  team
}

updateTeam <- function(team, game, i) {
  team$updateDate <- game$gameDate
  team$xp <- game$teamXP[i] + 1
  team$teamStr <- game$strNext[i, ]
  team$strNorm <- game$strNextNorm[i, ]
  team$strAgg <- game$strAggNext[i]
  team$isUpdated <- TRUE
  team
}

getTeamStrs <- function(team, rOptions) {
  if (!team$isUpdated) {
    teamStr <- rOptions$fTree[[team$fName]]
    strNorm=computeStrNorm(teamStr)
    strBetas <- rOptions$strBetas
    strAgg <- strNorm %*% strBetas
  }
  else {
    teamStr <- team$teamStr
    strNorm <-team$strNorm
    strAgg <- team$strAgg
  }

  teamStrs <- list(teamStr=teamStr, strNorm=strNorm, strAgg=strAgg)
  teamStrs
}

computeXP <- function(team, gameDate, k) {
  if (!team$isUpdated) {
    xp <- team$xp
  }
  else {
    t <- as.numeric(gameDate - team$updateDate)
    xp <- computeExpDecay(t, k / 365, team$xp)
  }

  xp
}
