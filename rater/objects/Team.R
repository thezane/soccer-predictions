newTeam <- function(teamName, fName) {
  team <- list(
    name=teamName,
    updateDate="",
    xp=0,
    fName=fName,
    teamStr=c(1, 1)
  )
  class(team) <- "Team"
  team
} 
