computeMeanGoals <- function(existsHA, rOptions) {
  meanGoals <- rOptions$meanGoals * c(1, 1)

  if (existsHA) {
    meanGoals[1] <- meanGoals[1] + rOptions$hA
  }

  meanGoals
}
