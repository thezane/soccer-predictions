computeMSE <- function(x1, x2) {
  costMat <- (x2 - x1) ^ 2
  mse <- sum(costMat)
  mse
} 
