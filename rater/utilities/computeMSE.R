computeMSE <- function(x1, x2) {
  costMat <- (x1 - x2) ^ 2
  mse <- sum(costMat)
  mse
} 
