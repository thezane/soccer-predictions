newTeam <- function(teamName, fName) {
  team <- list(
    name=teamName,
    fName=fName,
    teamStr=c(1, 1),
    isUpdated=FALSE,
    updateDate=as.Date("0001-01-01"),
    xp=0
  )
  class(team) <- "Team"
  team
} 
