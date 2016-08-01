newRatingsOptions <- function(fTree, tolRel, tolScale) {
  fNames <- keys(fTree)
  rOptions <- list(
    k=1,
    c=0.5,
    fTree=fTree,
    fNames=fNames,
    numFs=length(fNames),
    tolRel=tolRel,
    tolScale=tolScale
  )
  class(rOptions) <- "RatingsOptions"
  rOptions
}

updateOptions <- function(rOptions, k, c, strFs) {
  rOptions$k <- k
  rOptions$c <- c
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }
  
  rOptions
}
