computeStrNext <- function(teamStr, strPost, alphas) {
  alphas * strPost + (1 - alphas) * teamStr
}
