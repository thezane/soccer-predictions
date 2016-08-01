computeStr <- function(A, teamStr, c, tolRel, tolScale) {
  a <- teamStr[, 1];
  d <- teamStr[, 2];
  n <- nrow(A)
  I <- diag(n)
  A <- A + c * I[1: n, n: 1];

  while (TRUE) {
    strPost <- computeAD(A, a, d, tolScale);
    aPost <- strPost[["aPost"]]
    dPost <- strPost[["dPost"]]
    aDel <- aPost - a;
    dDel <- dPost - d;
    a <- aPost;
    d <- dPost;
    
    if (norm(aDel) < tolRel && norm(dDel) < tolRel) {
      strPost <- list(aPost=aPost, dPost=dPost);
      break;
    }
  }
  
  strPost
}

computeAD <- function(A, a, d, tolScale) {
  strRelA = scaleRating(A, a, tolScale);
  aRelA <- strRelA[["x"]]
  dRelA <- strRelA[["y"]]
  strRelD <- scaleRating(t(A), d, tolScale);
  dRelD <- strRelD[["x"]]
  aRelD <- strRelD[["y"]]
  aPost <- (aRelA + aRelD) / 2;
  dPost <- (dRelA + dRelD) / 2;
  strPost <- list(aPost=aPost, dPost=dPost)
  strPost
}

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
