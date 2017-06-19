computeLayerRatings <- function(game, rOptions, homeTeam, awayTeam,
    strPostNorm) {
  alphas <- game$weight * rOptions$k * game$reliability
  strNextNorm <- alphas * strPostNorm + (1 - alphas) * game$strNorm
  strNextNorm
}
