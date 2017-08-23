constructGames <- function(gTree, rOptions, dataPath) {
  dateFormat <- rOptions$dateFormat
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
    gameData <- addGame(T, i, rOptions,
        gTree, homeTeamName, awayTeamName, gameDate)
    gTree <- gameData[["gTree"]]
    tTree <- gameData[["tTree"]]
    game <- gameData[["game"]]
    i <- i + 1
  }
  
  gamesData <- list(gTree=gTree, T=T)
  gamesData
}

addGame <- function(T, i, rOptions, gTree, homeTeamName, awayTeamName,
      gameDate) {
  game <- new.Game(T, i, rOptions,
      homeTeamName, awayTeamName, gameDate)
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
