newTeam <- function(teamName, fName) {
  team <- list(
    name=teamName,
    updateDate="",
    xp=0,
    fName=fName,
    str=matrix(0, 2, 2)
  )
  class(team) <- "Team"
  team
} 
