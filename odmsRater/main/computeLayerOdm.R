computeLayerOdm <- function(A, strNorm, b, c) {
  teamStr <- exp(strNorm)
  a <- teamStr[, 1];
  d <- teamStr[, 2];
  teamStrFlipped <- matrix(c(a[1], a[2], d[2], d[1]), 2, 2)
  A <- b * A + c;
  strPostFlipped <- A %*% (1 / teamStrFlipped)
  aPostFlipped <- strPostFlipped[, 1]
  dPostFlipped <- strPostFlipped[, 2]
  strPost <- matrix(c(aPostFlipped[2], aPostFlipped[1],
      dPostFlipped[1], dPostFlipped[2]), 2, 2) 
  strPostNorm <- log(strPost)
  strPostNorm
}
