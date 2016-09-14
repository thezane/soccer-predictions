constructGames <- function(currentDate, tTree, currentContest,
    dateFormat, dataPath) {
  gTree <- hash()
  matchSrc <- paste(dataPath, "matches.csv", sep="")
  T <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- nrow(T)
  i <- 1
  
  while (i <= n) {
    gameDate <- as.Date(T[[i, "Date"]], dateFormat)
    
    if (gameDate <= currentDate) {
      homeTeamName <- T[[i, "HomeTeam"]]
      awayTeamName <- T[[i, "AwayTeam"]]
      gameData <- addGame(T, i, gTree, homeTeamName, awayTeamName,
          gameDate, currentContest)
      gTree <- gameData[["gTree"]]
      game <- gameData[["game"]]
      i <- i + 1
    }
    else {
      T <- T[-i, ]
      n <- n - 1
    }
  }
  
  gamesData <- list(gTree=gTree, T=T)
  gamesData
}

addGame <- function(T, i, gTree, homeTeamName, awayTeamName,
      gameDate, currentContest) {
  game <- newGame(T, i, homeTeamName, awayTeamName, gameDate,
      currentContest)
  gameDateStr <- game$gameDateStr
  
  if (!has.key(gameDateStr, gTree)) {
    gTree[gameDateStr] <- NULL
  }
  
  gDateList <- gTree[[gameDateStr]]
  game$gameNum <- length(gDateList) + 1
  gDateList[[game$gameNum]] <- game
  gTree[[gameDateStr]] <- gDateList
  gameData <- list(gTree=gTree, game=game)
  gameData
}
