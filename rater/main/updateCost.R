updateCost <- function(rOutput, rOptions, game, gamePrev) {
  homeTeam <- rOutput$tTree[[game$teamNames[1]]]
  awayTeam <- rOutput$tTree[[game$teamNames[2]]]
  teamStr <- game$teamStr  

  if (!game$isQualifier || homeTeam$fName != awayTeam$fName) {
    strPost <- game$teamStrPost
    alphas <- c(1, 1)
    strExpected <- computeStrNext(teamStr, strPost, alphas)
    rOutput <- updateRatingsCost(rOptions, rOutput,
        teamStr, strExpected)
  }

  if (!is.null(gamePrev) && gamePrev$isQualifier && game$isWorldCupGroup) {
    rOutput <- updateStrMedianCosts(rOutput)
  }

  rOutput
} 

updateRatingsCost <- function(rOptions, rOutput,
    teamStr, strExpected) {
  strNorm <- computeStrNorm(teamStr)
  strExpectedNorm <- computeStrNorm(strExpected)
  strCost <- computeMSE(strNorm, strExpectedNorm)
  rOutput$strCost <- rOutput$strCost + strCost
  rOutput
}
