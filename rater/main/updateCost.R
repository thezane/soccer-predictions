updateCost <- function(rOutput, game, gamePrev) {
  homeTeam <- rOutput$tTree[[game$teamNames[1]]]
  awayTeam <- rOutput$tTree[[game$teamNames[2]]]

  if (!game$isQualifier) {
    rOutput <- updateRatingsCost(rOutput, game)
  }

  if (!is.null(gamePrev) && gamePrev$isQualifier &&
        game$isWorldCupGroup) {
    rOutput <- updateStrMeanCosts(rOutput)
  }

  rOutput
} 

updateRatingsCost <- function(rOutput, game) {
  teamStr <- game$teamStr
  goalsNorm <- game$goalsNorm
  
  if (goalsNorm[1] != goalsNorm[2]) {
    strNorm <- computeStrNorm(teamStr)
    strTotal <- c(strNorm[1, 1] - strNorm[1, 2],
        strNorm[2, 1] - strNorm[2, 2])
    isCorrect <-
        goalsNorm[1] > goalsNorm[2] && strTotal[1] > strTotal[2] ||
        goalsNorm[1] < goalsNorm[2] && strTotal[1] < strTotal[2]
    strCost <- as.numeric(!isCorrect) - as.numeric(isCorrect)
    rOutput$strCost <- rOutput$strCost + strCost
  }
  
  rOutput
}
