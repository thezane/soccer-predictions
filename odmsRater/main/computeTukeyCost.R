# Apply the biweight function to list 'L'.
computeTukeyCost <- function(L) {
  sapply(L, function(x, c=4.685) {
      xAbs <- abs(x)

      if (xAbs <= c) {
        tukeyCost <- x ^ 6 / (6 * c ^ 4) - (x ^ 4) / (2 * c ^ 2) +
            x ^ 2 / 2
      }
      else {
        tukeyCost <- c ^ 2 / 6
      }

      tukeyCost
      })
}
