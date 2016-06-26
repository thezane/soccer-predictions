constructTeams <- function (matches) {
  teams <- hash()
  n <- nrow(matches)
  numTeams <- length(unique(matches[, "HomeTeam"]))
  i <- 1
  j <- 1
  
  while (i <= n && j <= numTeams) {
    homeTeam <- matches[i, "HomeTeam"]
    awayTeam <- matches[i, "AwayTeam"]
    
    if (!has.key(homeTeam, teams)) {
      teams <- addTeam(teams, matches[i, ], homeTeam, TRUE)
      j <- j + 1
    }
    
    if (!has.key(awayTeam, teams)) {
      teams <- addTeam(teams, matches[i, ], awayTeam, FALSE)
      j <- j + 1
    }   
    
    i <- i + 1
  }
  
  teams
}

addTeam <- function(teams, match, teamName, isHome) {
  prefix <- "Home"

  if (!isHome) {
    prefix <- "Away"
  }
  
  teams[teamName] <- c(match[paste(prefix, "AttackNext", sep="")],
      match[paste(prefix, "DefenseNext", sep="")])
  teams
}
