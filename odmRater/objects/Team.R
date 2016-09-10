newTeam <- function(teamName, i) {
  team <- list(
    name=teamName,
    teamStr=c(1, 1),
    strNext=c(1, 1),
    strAgg=1,
    strAggNext=1,
    i=i
  )
  
  class(team) <- "Team"
  team
} 

updateTeam <- function(team, x, y) {
  i <- team$i
  team$teamStr <- team$strNext
  team$strAgg <- team$strAggNext
  team$strNext <- c(x[i], y[i])
  team$strAggNext <- x[i] / y[i]
  team
}

updateStrNext <- function(team, x, y) {
  i <- team$i
  team$strNext <- c(x[i], y[i])
  team$strAggNext <- x[i] / y[i]
  team 
}
