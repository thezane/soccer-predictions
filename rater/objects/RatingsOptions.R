newRatingsOptions <- function(fTree, tolRel, tolScale) {
  fNames <- keys(fTree)
  rOptions <- list(
    k=0.8,
    c=0.4,
    xpDefault=1,
    fTree=fTree,
    fNames=fNames,
    numFs=length(fNames),
    tolRel=tolRel,
    tolScale=tolScale
  )
  strFsNorm <- c(-0.3, -0.5, 0, -0.3, -0.6, 0.3)
  strFs <- exp(strFsNorm)
  class(rOptions) <- "RatingsOptions"
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }

  rOptions
}

updateOptions <- function(rOptions, k, c) {
  rOptions$k <- k
  rOptions$c <- c
  rOptions
}
