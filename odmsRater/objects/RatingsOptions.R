newRatingsOptions <- function(fTree) {
  fNames <- keys(fTree)
  numFs <- length(fNames)

  rOptions <- list(
    # ODM layer
    b=0.3,
    c=0.3,

    # Lin layer
    k=0.16,

    # Poisson layer
    meanGoals=1,
    strBeta=1.6,
    hA=0.4,
    corrBeta=-1,   

    # Lower bounds for optimizable parameters
    bLBd=0.01,
    cLBd=0.01,
    kLBd=0,
    meanGoalsLBd=0.01,
    strBetaLBd=0.01,
    hALBd=0,
    corrBetaLBd=-Inf,
    
    # Upper bounds for optimizable parameters
    bUBd=Inf,
    cUBd=Inf,
    kUBd=1,
    meanGoalsUBd=Inf,
    strBetaUBd=Inf,
    hAUBd=Inf,
    corrBetaUBd=0,
    
    # Default federation strengths for Africa, Asia, Europe,
    # North America, Oceania and South America respectively
    strFsNorm=c(0, -0.28, 0.28, -0.24, -0.6, 0.48),
    
    # Non-optimizable paramters
    fTree=fTree,
    fNames=fNames,
    minUpdates=0,
    numFs=numFs,
    odmIter=10,
    
    # Regularization
    slopeCost=0,
    strMeanCostReg=0.1,
    slopeCostReg=0.001,

    # L-BFGS-B parameters
    factr=1e-04 / .Machine$double.eps,
    lmm=10
  )
  
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  class(rOptions) <- "RatingsOptions"
  rOptions
}

getModel <- function(rOptions) {
  c(rOptions$b, rOptions$c,
    rOptions$k,
    rOptions$meanGoals, rOptions$strBeta, rOptions$hA,
        rOptions$corrBeta)
}

getModelLBd <- function(rOptions) {
  c(rOptions$bLBd, rOptions$cLBd,
    rOptions$kLBd,
    rOptions$meanGoalsLBd, rOptions$strBetaLBd, rOptions$hALBd,
        rOptions$corrBetaLBd)
}

getModelUBd <- function(rOptions) {
  c(rOptions$bUBd, rOptions$cUBd,
    rOptions$kUBd,
    rOptions$meanGoalsUBd, rOptions$strBetaUBd, rOptions$hAUBd,
        rOptions$corrBetaUBd)
}

updateOptions <- function(rOptions, x) {
  rOptions$b <- x[1]
  rOptions$c <- x[2]
  rOptions$k <- x[3]
  rOptions$meanGoals <- x[4]
  rOptions$strBeta <- x[5]
  rOptions$hA <- x[6]
  rOptions$corrBeta <- x[7]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$slopeCost <- rOptions$slopeCostReg *
      norm(matrix(c(rOptions$b, rOptions$strBeta, rOptions$hA)), "f")
  rOptions
}

printModel <- function(rOptions) {
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
}
