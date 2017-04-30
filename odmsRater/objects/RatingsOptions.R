newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    # Optimizable parameters
    c=0.1,
    kQ=0.1,
    kT=0.2,
    meanGoals=1,
    corrBeta=-1,
    hA=0.2,
    strBeta=0.8,
    
    # Lower bounds for parameters
    cLBd=0.01,
    kQLBd=0.01,
    kTLBd=0.01,
    meanGoalsLBd=0.01,
    corrBetaLBd=-Inf,
    hALBd=0,
    strBetaLBd=0.01,
    
    # Upper bounds for parameters
    cUBd=Inf,
    kQUBd=1,
    kTUBd=1,
    meanGoalsUBd=Inf,
    corrBetaUBd=0,
    hAUBd=Inf,
    strBetaUBd=Inf,
    
    # Non-optimizable paramters
    fTree=fTree,
    fNames=fNames,
    numFs=numFs,
    tolOpt=0.01,
    tolRel=0.01,
    tolScale=0.01
  )
  
  class(rOptions) <- "RatingsOptions"
  rOptions
}

updateOptions <- function(rOptions, c, ks, biasBetas, featureBetas,
      strFsNorm) {
  rOptions$c <- c
  rOptions$kQ <- ks[1]
  rOptions$kT <- ks[2]
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
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("kQ = %f", rOptions$kQ)))
  print(noquote(sprintf("kT = %f", rOptions$kT)))
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
