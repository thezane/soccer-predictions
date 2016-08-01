constructGames <- function(currentDate, dateFormat, tTree,
    dataPath) {
  gTree <- hash()
  hA <- newHomeAdvantage()
  matchSrc <- paste(dataPath, "matches.csv", sep="")
  T <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- ncol(T)
  i <- 1
  
  while (i <- n) {
    gameDate <- as.Date(T[[i, "Date"]], dateFormat)
    
    if (gameDate <= currentDate) {
      homeTeamName <- T[[i, "HomeTeam"]]
      awayTeamName <- T[[i, "AwayTeam"]]
      gameData <- addGame(T, i, gTree, homeTeamName, awayTeamName,
          gameDate)
      gTree <- gameData[["gTree"]]
      game <- gameData[["game"]]
      hA <- updateHA(hA, game)
    }
    else {
      T <- T[-i, ]
      n <- n - 1
    }
    
    i <- i + 1
  }
  
  gamesData <- list(gTree=gTree, T=T, hA=hA)
  gamesData
}

addGame <- function(T, i, gTree, homeTeamName, awayTeamName,
    gameDate) {
  game <- newGame(T, i, homeTeamName, awayTeamName, gameDate)
  gameDateStr <- game$gameDateStr
  
  if (!has.key(gameDateStr, gTree)) {
    gTree[gameDateStr] <- c()
  }
  
  game$i <- length(gTree[[gameDateStr]]) + 1
  gTree[[gameDateStr]] <- c(gTree[[gameDateStr]], game)
  gameData <- list(gTree=gTree, game=game)
  gameData
}
