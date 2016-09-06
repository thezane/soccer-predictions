newTeam <- function(teamName, fName) {
  team <- list(
    name=teamName,
    fName=fName,
    teamStr=c(1, 1),
    isUpdated=FALSE,
    updateDate=as.Date("0001-01-01"),
    alpha=0.5,
    xp=1
  )
  class(team) <- "Team"
  team
} 

resetTeam <- function(team) {
  team$teamStr <- c(1, 1)
  team$strNorm <- c(0, 0)
  team$isUpdated <- FALSE
  team$updateDate <- as.Date("0001-01-01")
  team$alpha <- 0.5
  team$xp <- 1
  team
}

updateTeam <- function(team, game, i) {
  team$updateDate <- game$gameDate
  team$xp <- game$teamXP[i]
  team$teamStr <- game$strNext[i, ]
  team$isUpdated <- TRUE
  team
}

computeTeamStr <- function(team, fTree) {
  if (!team$isUpdated) {
    teamStr <- fTree[[team$fName]]
  }
  else {
    teamStr <- team$teamStr
  }

  teamStr
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
