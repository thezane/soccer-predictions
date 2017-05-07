computeLayerLin <- function(game, rOptions, homeTeam, awayTeam,
    strPostNorm) {
  alphas <- c(computeAlpha(homeTeam, rOptions, game),
      computeAlpha(awayTeam, rOptions, game))
  strNextNorm <- alphas * strPostNorm + (1 - alphas) * game$strNorm
  strNextNorm
}
