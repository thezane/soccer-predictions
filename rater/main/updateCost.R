updateCost <- function(rOutput, rOptions, game) {
  homeTeam <- rOutput$tTree[[game$teamNames[1]]]
  awayTeam <- rOutput$tTree[[game$teamNames[2]]]
  teamStr <- game$teamStr
  rOutput <- updateStrAll(rOutput, teamStr)
  
  if (!game$isQualifier || homeTeam$fName != awayTeam$fName) {
    strPost <- game$teamStrPost
    alphas <- c(0.5, 0.5)
    strExpected <- computeStrNext(teamStr, strPost, alphas)
    rOutput <- updateRatingsCost(rOptions, rOutput,
        teamStr, strExpected)
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
