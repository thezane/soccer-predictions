# Compute next game strengths for teams in a game with model parameters
# rOptions and post-game strengths.
computeLayerRatings <- function(game, rOptions, strPostNorm) {
  alphas <- game$reliability * game$weightContest * rOptions$k
  strNextNorm <- alphas * strPostNorm + (1 - alphas) * game$strNorm
  strNextNorm
}
