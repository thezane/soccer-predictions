resetRatings <- function(tTree) {
  teams <- keys(tTree)
  n <- length(teams)
  i <- 1
  
  while (i <= n) {
    teamName <- teams[i]
    team <- tTree[[teamName]]
    team$teamstr <- c(1, 1)
    team$updateDate <- ""
    team$xp <- 0
    tTree[teamName] <- team
    i <- i + 1
  }
  
  tTree
} 
