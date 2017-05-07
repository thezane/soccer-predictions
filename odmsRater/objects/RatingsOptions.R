newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    # Optimizable parameters
    b=0.8,
    c=0.3,
    meanGoals=1,
    strBeta=0.8,
    hA=0.2,
    corrBeta=-1,

    # Default federation strengths for Africa, Asia, Europe,
    # North America, Oceania and South America respectively
    strFsNorm=c(-0.2, -0.2, 0.4, -0.2, -0.6, 0.8),    

    # Lower bounds for parameters
    bLBd=0.01,
    cLBd=0.01,
    meanGoalsLBd=0.01,
    strBetaLBd=0.01,
    hALBd=0,
    corrBetaLBd=-Inf,
    
    # Upper bounds for parameters
    bUBd=Inf,
    cUBd=Inf,
    meanGoalsUBd=Inf,
    strBetaUBd=Inf,
    hAUBd=Inf,
    corrBetaUBd=0,
    
    # Non-optimizable paramters
    kQ=0.1,
    kT=0.15,
    fTree=fTree,
    fNames=fNames,
    numFs=numFs,
    factr=0.01,
    tolRel=0.01,
    tolScale=0.01
  )
  
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$strFsNormLBd <- rep(-Inf, rOptions$numFs)
  rOptions$strFsNormUBd <- rep(Inf, rOptions$numFs)
  class(rOptions) <- "RatingsOptions"
  rOptions
}

updateOptions <- function(rOptions, x) {
  rOptions$b <- x[1]
  rOptions$c <- x[2]
  rOptions$meanGoals <- x[3]
  rOptions$strBeta <- x[4]
  rOptions$hA <- x[5]
  rOptions$corrBeta <- x[6]
  strFs <- exp(x[-c(1: 6)])
  i <- 1
  
  while (i <= rOptions$numFs) {
    strF <- strFs[i]
    rOptions$fTree[[rOptions$fNames[i]]] <- c(strF, 1 / strF)
    i <- i + 1
  }

  rOptions
}

getModel <- function(rOptions) {
  c(rOptions$b, rOptions$c,
    rOptions$meanGoals, rOptions$strBeta, rOptions$hA,
        rOptions$corrBeta,
    rOptions$strFsNorm)
}

getModelLBd <- function(rOptions) {
  c(rOptions$bLBd, rOptions$cLBd,
    rOptions$meanGoalsLBd, rOptions$strBetaLBd, rOptions$hALBd,
        rOptions$corrBetaLBd,
    rOptions$strFsNormLBd)
}

getModelUBd <- function(rOptions) {
  c(rOptions$bUBd, rOptions$cUBd,
    rOptions$meanGoalsUBd, rOptions$strBetaUBd, rOptions$hAUBd,
        rOptions$corrBetaUBd,
    rOptions$strFsNormUBd)
}

printModel <- function(rOptions) {
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  i <- 1
  
  while (i <= rOptions$numFs) {
    print(noquote(sprintf("%s = %f", rOptions$fNames[i],
        log(rOptions$fTree[[rOptions$fNames[i]]][1]))))
    i <- i + 1
  }
}
