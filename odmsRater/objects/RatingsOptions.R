newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    # ODM layer
    b=0.8,
    c=0.3,

    # Lin layer
    kQ=0.1,
    kT=0.15,

    # Poisson layer
    meanGoals=1,
    strBeta=0.8,
    hA=0.2,
    corrBeta=-1,

    # Default federation strengths for Africa, Asia, Europe,
    # North America, Oceania and South America respectively
    strFsNorm=c(-0.2, -0.2, 0.2, -0.2, -0.6, 0.6),    

    # Lower bounds for optimizable parameters
    bLBd=0.01,
    cLBd=0.01,
    kQLBd=0,
    kTLBd=0,
    meanGoalsLBd=0.01,
    strBetaLBd=0.01,
    hALBd=0,
    corrBetaLBd=-Inf,
    
    # Upper bounds for optimizable parameters
    bUBd=Inf,
    cUBd=Inf,
    kQUBd=1,
    kTUBd=1,
    meanGoalsUBd=Inf,
    strBetaUBd=Inf,
    hAUBd=Inf,
    corrBetaUBd=0,
    
    # Non-optimizable paramters
    fTree=fTree,
    fNames=fNames,
    numFs=numFs,
    tolRel=0.01,
    tolScale=0.01,

    # L-BFGS-B parameters
    factr=1e-04 / .Machine$double.eps,
    lmm=10
  )
  
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$strFsNormLBd <- rep(-Inf, rOptions$numFs)
  rOptions$strFsNormUBd <- rep(Inf, rOptions$numFs)
  class(rOptions) <- "RatingsOptions"
  rOptions
}

getModel <- function(rOptions) {
  c(rOptions$b, rOptions$c,
    rOptions$kQ, rOptions$kT,
    rOptions$meanGoals, rOptions$strBeta, rOptions$hA,
        rOptions$corrBeta,
    rOptions$strFsNorm)
}

getModelLBd <- function(rOptions) {
  c(rOptions$bLBd, rOptions$cLBd,
    rOptions$kQLBd, rOptions$kTLBd,
    rOptions$meanGoalsLBd, rOptions$strBetaLBd, rOptions$hALBd,
        rOptions$corrBetaLBd,
    rOptions$strFsNormLBd)
}

getModelUBd <- function(rOptions) {
  c(rOptions$bUBd, rOptions$cUBd,
    rOptions$kQUBd, rOptions$kTUBd,
    rOptions$meanGoalsUBd, rOptions$strBetaUBd, rOptions$hAUBd,
        rOptions$corrBetaUBd,
    rOptions$strFsNormUBd)
}

getModelSlopes <- function(rOptions) {
  c(rOptions$b,
    rOptions$kQ, rOptions$kT,
    rOptions$strBeta, rOptions$hA)
}

updateOptions <- function(rOptions, x) {
  rOptions$b <- x[1]
  rOptions$c <- x[2]
  rOptions$kQ <- x[3]
  rOptions$kT <- x[4]
  rOptions$meanGoals <- x[5]
  rOptions$strBeta <- x[6]
  rOptions$hA <- x[7]
  rOptions$corrBeta <- x[8]
  rOptions$strFsNorm <- x[-c(1: 8)]
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
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("kQ = %f", rOptions$kQ)))
  print(noquote(sprintf("kT = %f", rOptions$kT)))
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
