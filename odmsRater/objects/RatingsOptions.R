newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    k=1,
    c=0.2,
    meanGoals=1,
    corrBeta=-1,
    hA=0.2,
    strBeta=1,
    kLBd=0,
    cLBd=0.01,
    meanGoalsLBd=0,
    corrBetaLBd=-Inf,
    hALBd=0,
    strBetaLBd=0,
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

updateOptions <- function(rOptions, k, c, biasBetas, featureBetas,
    strFsNorm) {
  rOptions$k <- k
  rOptions$c <- c
  rOptions$meanGoals <- biasBetas[1]
  rOptions$corrBeta <- biasBetas[2]
  rOptions$hA <- featureBetas[1]
  rOptions$strBeta <- featureBetas[2]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  strFs <- exp(strFsNorm)
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }

  rOptions
}

printModel <- function(rOptions) {
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  print(noquote(sprintf("str = %f", rOptions$strBeta)))
  i <- 1
  
  while (i <= rOptions$numFs) {
    print(noquote(sprintf("%s = %f", rOptions$fNames[i],
        log(rOptions$fTree[[rOptions$fNames[i]]][1]))))
    i <- i + 1
  }
}
