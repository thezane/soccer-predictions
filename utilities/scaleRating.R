scaleRating <- function(A, x, odmIter) {
  y <- A %*% (1 / x);
  i <- 1
 
  while (i <= odmIter) {
    xPost <- t(A) %*% (1 / y);
    yPost <- A %*% (1 / x);
    x <- xPost;
    y <- yPost;
    i <- i + 1
  }
  
  v <- list(x=x, y=y)
  v
}
