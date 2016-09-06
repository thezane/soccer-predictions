newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    ks=c(1.5, 1.5),
    c=0.2,
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

updateOptions <- function(rOptions, ks, c, strBetas, strFsNorm) {
  rOptions$ks <- ks
  rOptions$c <- c
  rOptions$model <- updateModel(rOptions$model, strBetas)
  strFs <- exp(strFsNorm)
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }

  rOptions
}
