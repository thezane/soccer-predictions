newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    # Goals layer
    s=1,
    m=1,
  
    # ODM layer
    b=0.5,
    c=0.5,

    # Lin layer
    k=0.16,

    # Poisson layer
    meanGoals=1,
    strBeta=2,
    hA=0.3,
    corrBeta=-1,

    # Default federation strengths for Africa, Asia, Europe,
    # North America, Oceania and South America respectively
    strFsNorm=c(0, 0, 0.2, 0, -0.6, 0.4),    

    # Lower bounds for optimizable parameters
    sLBd=-Inf,
    mLBd=-Inf,
    bLBd=0.01,
    cLBd=0,
    kLBd=0,
    meanGoalsLBd=0.01,
    strBetaLBd=0.01,
    hALBd=0,
    corrBetaLBd=-Inf,
    
    # Upper bounds for optimizable parameters
    sUBd=Inf,
    mUBd=Inf,
    bUBd=Inf,
    cUBd=Inf,
    kUBd=1,
    meanGoalsUBd=Inf,
    strBetaUBd=Inf,
    hAUBd=Inf,
    corrBetaUBd=0,
    
    # Non-optimizable paramters
    fTree=fTree,
    fNames=fNames,
    minUpdates=0,
    numFs=numFs,
    tolRel=0.01,
    tolScale=0.01,

    # L-BFGS-B parameters
    factr=1e-06 / .Machine$double.eps,
    lmm=10
  )
  
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$strFsNormLBd <- rep(-Inf, rOptions$numFs)
  rOptions$strFsNormUBd <- rep(Inf, rOptions$numFs)
  class(rOptions) <- "RatingsOptions"
  rOptions
}

getModel <- function(rOptions) {
  c(rOptions$s, rOptions$m,
    rOptions$b, rOptions$c,
    rOptions$k,
    rOptions$meanGoals, rOptions$strBeta, rOptions$hA,
        rOptions$corrBeta,
    rOptions$strFsNorm)
}

getModelLBd <- function(rOptions) {
  c(rOptions$sLBd, rOptions$mLBd,
    rOptions$bLBd, rOptions$cLBd,
    rOptions$kLBd,
    rOptions$meanGoalsLBd, rOptions$strBetaLBd, rOptions$hALBd,
        rOptions$corrBetaLBd,
    rOptions$strFsNormLBd)
}

getModelUBd <- function(rOptions) {
  c(rOptions$sUBd, rOptions$mUBd,
    rOptions$bUBd, rOptions$cUBd,
    rOptions$kUBd,
    rOptions$meanGoalsUBd, rOptions$strBetaUBd, rOptions$hAUBd,
        rOptions$corrBetaUBd,
    rOptions$strFsNormUBd)
}

getModelSlopes <- function(rOptions) {
  c(rOptions$s,
    rOptions$b,
    rOptions$k,
    rOptions$strBeta, rOptions$hA)
}

updateOptions <- function(rOptions, x) {
  rOptions$s <- x[1]
  rOptions$m <- x[2]
  rOptions$b <- x[3]
  rOptions$c <- x[4]
  rOptions$k <- x[5]
  rOptions$meanGoals <- x[6]
  rOptions$strBeta <- x[7]
  rOptions$hA <- x[8]
  rOptions$corrBeta <- x[9]
  rOptions$strFsNorm <- x[-c(1: 9)]
  strFsNorm <- rOptions$strFsNorm
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  i <- 1
  
  while (i <= rOptions$numFs) {
    strFNorm <- strFsNorm[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strFNorm, -strFNorm)
    i <- i + 1
  }

  rOptions
}

printModel <- function(rOptions) {
  print(noquote(sprintf("s = %f", rOptions$s)))
  print(noquote(sprintf("m = %f", rOptions$m)))
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  i <- 1
  
  while (i <= rOptions$numFs) {
    print(noquote(sprintf("%s = %f", rOptions$fNames[i],
        rOptions$fTree[[rOptions$fNames[i]]][1])))
    i <- i + 1
  }
}
