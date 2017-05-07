viewStrNext <- function(strNorm, b, c) {
  source("computeStr.R")
  source("../../utilities/scaleRating.R")
  maxGoals <- 5
  tolRel <- 0.01
  tolScale <- 0.01
  teamStr <- exp(strNorm)
  i <- 0

  while (i <= maxGoals) {
    j <- i

    while (j <= maxGoals) {
      print(c(j, i))
      A <- matrix(c(0, j, i, 0), 2)
      print(log(computeStr(A, teamStr, b, c, tolRel, tolScale)))
      cat("\n")
      j <- j + 1
    }

    i <- i + 1
  }
}
