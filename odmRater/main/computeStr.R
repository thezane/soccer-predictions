computeStr <- function(A, n, c, tolScale) {
  A <- A + c
  x <- rep(1, n)
  v <- scaleRating(A, x, tolScale)
  v
}
