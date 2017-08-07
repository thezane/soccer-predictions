constructGames <- function(currentDate, dateFormat, dataPath) {
  gTree <- hash()
  matchSrc <- paste(dataPath, "matches.csv", sep="")
  T <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  T <- T[order(as.Date(T[["Date"]], format=dateFormat)), ]
  n <- nrow(T)
  i <- 1
  
  while (i <= n) {
    gameDate <- as.Date(T[[i, "Date"]], dateFormat)  
    homeTeamName <- T[[i, "HomeTeam"]]
    awayTeamName <- T[[i, "AwayTeam"]]
    gameData <- addGame(T, i, gTree, homeTeamName, awayTeamName,
        currentDate, gameDate)
    gTree <- gameData[["gTree"]]
    tTree <- gameData[["tTree"]]
    game <- gameData[["game"]]
    i <- i + 1
  }
  
  gamesData <- list(gTree=gTree, T=T)
  gamesData
}

addGame <- function(T, i, gTree, homeTeamName, awayTeamName,
      currentDate, gameDate) {
  game <- newGame(T, i, homeTeamName, awayTeamName,
      currentDate, gameDate)
  gameDateStr <- game$gameDateStr
  
  if (!has.key(gameDateStr, gTree)) {
    gTree[[gameDateStr]] <- NULL
  }
  
  gDateList <- gTree[[gameDateStr]]
  game$gameNum <- length(gDateList) + 1
  gDateList[[game$gameNum]] <- game
  gTree[[gameDateStr]] <- gDateList
  gameData <- list(gTree=gTree, game=game)
  gameData
}
