new.GameHypo <- function(homeTeamName, awayTeamName, existsHa, rData) {
  rOptions <- rData[["rOptions"]]
  rOutput <- rData[["rOutput"]]
  tTree <- rOutput$tTree
  homeTeam <- tTree[[homeTeamName]]
  awayTeam <- tTree[[awayTeamName]]
  game <- new.Game(homeTeamName, awayTeamName, existsHa, rOptions)
  game$strNorm <- matrix(c(homeTeam$strNorm, awayTeam$strNorm), 2, 2, TRUE)
  game$strNormBeta <- matrix(c(homeTeam$strNormBeta, awayTeam$strNormBeta),
        2, 2, TRUE)
  game$strAgg <- c(homeTeam$strAgg, awayTeam$strAgg)
  
  class(gameHypo) <- c("Game", class(game))
  gameHypo
}
