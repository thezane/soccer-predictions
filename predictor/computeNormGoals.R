computeNormGoals <- function(x) {
  x <- min(2, x) + log(1 + max(0, x - 2))
  x
} 
