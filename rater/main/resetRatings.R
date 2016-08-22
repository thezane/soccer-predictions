resetRatings <- function(tTree, rOptions) {
  teams <- keys(tTree)
  n <- length(teams)
  i <- 1
  
  while (i <= n) {
    teamName <- teams[i]
    team <- tTree[[teamName]]
    team$teamstr <- c(1, 1)
    team$isUpdated <- FALSE
    team$xp <- rOptions$xpDefault
    tTree[teamName] <- team
    i <- i + 1
  }
  
  tTree
} 
