rateTeams <- function(x, rOptions, rOutput, lambdas=rep(1, 4)) {
  print(x)
  strFsNorm <- x[-c(1: 6)]
  rOptions <- updateOptions(rOptions, x[c(1, 2)], x[3], x[c(4, 5)],
      x[6], strFsNorm)
  rOutput <- getRatings(rOptions, rOutput)
  strCost <- rOutput$strCost
  goalsCost <- computeGoalsCost(rOutput)
  strMeanCost <- computeStrMeanCost(rOutput)
  strFsNormCost <- norm(matrix(strFsNorm), "f")
  xpCost <- rOutput$xpCost
  goalsReg <- lambdas[1] * min(0, 1.1 - goalsCost) ^ 2
  strReg <- lambdas[2] * (min(0, 0.05 - strMeanCost[1]) ^ 2 +
      min(0, 0.05 - strMeanCost[1]) ^ 2)
  strFsNormReg <- lambdas[3] * min(0, 1 - strFsNormCost) ^ 2
  xpReg <- lambdas[4] * xpCost
  print(c(strCost, goalsReg, strReg, strFsNormReg, xpCost))
  rOutput$y <- strCost + goalsReg + strReg + strFsNormReg + xpReg
  rData <- list(rOptions=rOptions, rOutput=rOutput)
  rData
}

getRatings <- function(rOptions, rOutput) {
  tTree <- rOutput$tTree
  gTree <- rOutput$gTree
  gi <- rOutput$gi
  tTree <- resetRatings(tTree)
  gi <- reset(gi)
  gamePrev <- NULL
  i <- 1
  
  while (hasNextGame(gi)) {
    gameData <- nextGame(gi)
    gi <- gameData[["gi"]]
    game <- gameData[["game"]]
    strPrereqs <- computeStrPrereqs(tTree, game, rOptions)
    updateStrData <- updateStr(strPrereqs, rOptions)
    tTree <- updateStrData[["tTree"]]
    game <- updateStrData[["game"]]
    costData <- updateCost(rOptions, rOutput, game, gamePrev)
    rOutput <- costData[["rOutput"]]
    game <- costData[["game"]]
    gDateList <- gTree[[game$gameDateStr]]
    gDateList[[game$gameNum]] <- game
    gTree[game$gameDateStr] <- gDateList
    gamePrev <- game
    i <- i + 1
  }
  
  rOutput$tTree <- tTree
  rOutput$gTree <- gTree
  rOutput$gi <- gi
  rOutput
}

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

computeStrPrereqs <- function(tTree, game, rOptions) {
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  fTree <- rOptions$fTree 
  game <- updateGamePreRate(game, fTree, rOptions$ks,
      homeTeam, awayTeam)
  strPrereqs <- list(game=game, tTree=tTree)
  strPrereqs
}

updateStr <- function(strPrereqs, rOptions) {
  game <- strPrereqs[["game"]]
  tTree <- strPrereqs[["tTree"]]
  strPost <- computeStr(game$A, game$teamStr, rOptions$c,
      rOptions$tolRel, rOptions$tolScale)
  game <- updateGamePostRate(game, strPost)
  homeTeamName <- game$teamNames[1]
  awayTeamName <- game$teamNames[2]
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]] 
  tTree[homeTeamName] <- updateTeam(homeTeam, game, 1)
  tTree[awayTeamName] <- updateTeam(awayTeam, game, 2)
  updateStrData <- list(tTree=tTree, game=game)
  updateStrData
}
