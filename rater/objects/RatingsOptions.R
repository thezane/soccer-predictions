newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    ks=c(1, 1),
    c=0.5,
    model=newModel(),
    xpDefault=1,
    fTree=fTree,
    fNames=fNames,
    numFs=numFs,
    tolRel=0.01,
    tolScale=0.01
  )
  
  class(rOptions) <- "RatingsOptions"
  rOptions
}

updateOptions <- function(rOptions, ks, c, aBeta, dBeta, corrBeta,
    p, strFsNorm) {
  rOptions$ks <- ks
  rOptions$c <- c
  rOptions$model <- updateModel(rOptions$model, aBeta, dBeta, corrBeta,
      p)
  strFs <- exp(strFsNorm)
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }

  rOptions
}
