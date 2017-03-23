scaleRating <- function(A, x, tolScale) {
  y <- A %*% (1 / x);
 
  while (TRUE) {
    xPost = t(A) %*% (1 / y);
    yPost = A %*% (1 / x);
    xDel <- xPost - x;
    yDel <- yPost - y;
    x <- xPost;
    y <- yPost;

    if (norm(xDel) < tolScale && norm(yDel) < tolScale) {
      break;
    }
  }
  
  v <- list(x=x, y=y)
  v
}
