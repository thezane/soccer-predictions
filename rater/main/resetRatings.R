resetRatings <- function(tTree) {
  teams <- keys(tTree)
  n <- length(teams)
  i <- 1
  
  while (i <= n) {
    teamName <- teams[i]
    team <- tTree[[teamName]]
    tTree[teamName] <- resetTeam(team)
    i <- i + 1
  }
  
  tTree
} 
