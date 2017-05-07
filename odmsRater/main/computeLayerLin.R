computeLayerLin <- function(game, rOptions, homeTeam, awayTeam,
    strPost) {
  alphas <- c(computeAlpha(homeTeam, rOptions, game),
      computeAlpha(awayTeam, rOptions, game))
  strNext <- alphas * strPost + (1 - alphas) * game$teamStr
  strNext
}
