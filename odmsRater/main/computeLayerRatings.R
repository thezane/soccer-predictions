computeLayerRatings <- function(game, rOptions, homeTeam, awayTeam,
    strPostNorm) {
  alphas <- game$weight * rOptions$k * c(1, 1)
  strNextNorm <- alphas * strPostNorm + (1 - alphas) * game$strNorm
  strNextNorm
}
