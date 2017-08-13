computeLayerRatings <- function(game, rOptions, homeTeam, awayTeam,
    strPostNorm) {
  alphas <- game$reliability * game$weight * rOptions$k
  strNextNorm <- alphas * strPostNorm + (1 - alphas) * game$strNorm
  strNextNorm
}
