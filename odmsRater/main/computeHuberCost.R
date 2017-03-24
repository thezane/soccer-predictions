# Apply huber function to list 'L'.
computeHuberCost <- function(L) {
  sapply(L, function(x, c=1.345) {
      xAbs <- abs(x)

      if (xAbs <= c) {
        huberCost <- x ^ 2
      }
      else {
        huberCost <- c * (2 * xAbs - c)
      }

      huberCost
      })
}
