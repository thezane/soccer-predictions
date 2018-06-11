new.RatingsOptions <- function() {
  rOptions <- list(
    # Ha layer
    meanGoals=1,
    haBias=0.4,

    # ODM layer
    b=0.3,
    c=0.3,

    # Ratings layer
    k=0.16,

    # Bivariate Poisson layer
    strBeta=1.6,
    corrBeta=-1,
    
    # Poisson layer
    theta=1,
    
    # Mixture layer
    tieBias=-3.2,
    tieBeta=-0.8,  

    # Lower bounds for optimizable parameters
    meanGoalsLBd=0.01,
    haBiasLBd=0.01,
    bLBd=0.01,
    cLBd=0.01,
    kLBd=0,
    strBetaLBd=0.01,
    corrBetaLBd=-Inf,
    thetaLBd=0.01,
    tieBiasLBd=-Inf,
    tieBetaLBd=-Inf,
    
    # Upper bounds for optimizable parameters
    meanGoalsUBd=Inf,
    haBiasUBd=Inf,
    bUBd=Inf,
    cUBd=Inf,
    kUBd=1,
    strBetaUBd=Inf,
    corrBetaUBd=0,
    thetaUBd=Inf,
    tieBiasUBd=Inf,
    tieBetaUBd=0,
    
    # Non-optimizable paramters
    fTree=list(
        "Africa"=c(0.04, -0.04),
        "Asia"=c(-0.2, 0.2),
        "Europe"=c(0.28, -0.28),
        "North America"=c(-0.2, 0.2),
        "Oceania"=c(-0.52, 0.52),
        "South America"=c(0.48, -0.48)),
    wTree=list(
        "very high"=1,
        "high"=5/6,
        "medium"=4/6,
        "low"=3/6,
        "very low"=2/6
    ),
    dateFormat="%Y-%m-%d",
    isFullTime=FALSE,
    isOptimized=FALSE,
    iterName="odms-iter",
    minUpdatesUntilReliable=20,
    odmIter=1,
    pGoalsMatSizeBase=20,
    writeName="odms-matches",
    
    # Regularization
    slopeCost=0,
    strMeanCostReg=0.1,
    slopeCostReg=0.001,

    # L-BFGS-B parameters
    factr=1e-05 / .Machine$double.eps,
    lmm=10
  )

  rOptions$currentDate <- as.Date("2003-01-01", rOptions$dateFormat)
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$layersComputer <- computeLayers.RatingsOptions

  class(rOptions) <- "RatingsOptions"
  rOptions
}

getModel <- function(class) {
  UseMethod("getModel")
}

getModel.RatingsOptions <- function(rOptions) {
  c(rOptions$meanGoals, rOptions$haBias,
      rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$strBeta, rOptions$corrBeta,
      rOptions$theta, rOptions$tieBias, rOptions$tieBeta)
}

getModelLBd <- function(class) {
  UseMethod("getModelLBd")
}

getModelLBd.RatingsOptions <- function(rOptions) {
  c(rOptions$meanGoalsLBd, rOptions$haBiasLBd,
      rOptions$bLBd, rOptions$cLBd,
      rOptions$kLBd,
      rOptions$strBetaLBd, rOptions$corrBetaLBd,
      rOptions$thetaLBd, rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

getModelUBd <- function(class) {
  UseMethod("getModelUBd")
}

getModelUBd.RatingsOptions <- function(rOptions) {
  c(rOptions$meanGoalsUBd, rOptions$haBiasUBd,
      rOptions$bUBd, rOptions$cUBd,
      rOptions$kUBd,
      rOptions$strBetaUBd, rOptions$corrBetaUBd,
      rOptions$thetaUBd, rOptions$tieBiasUBd, rOptions$tieBetaUBd)
}

getSlopes <- function(class) {
  UseMethod("getSlopes")
}

getSlopes.RatingsOptions <- function(rOptions) {
  matrix(c(rOptions$haBias,
      rOptions$b,
      rOptions$strBeta,
      rOptions$tieBeta))
}

update <- function(class, x) {
  UseMethod("update")
}

update.RatingsOptions <- function(rOptions, x) {
  rOptions$meanGoals <- x[1]
  rOptions$haBias <- x[2]
  rOptions$b <- x[3]
  rOptions$c <- x[4]
  rOptions$k <- x[5]
  rOptions$strBeta <- x[6]
  rOptions$corrBeta <- x[7]
  rOptions$theta <- x[8]
  rOptions$tieBias <- x[9]
  rOptions$tieBeta <- x[10]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg * t(slopes) %*% slopes
  rOptions
}

print.RatingsOptions <- function(rOptions) {
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("haBias = %f", rOptions$haBias)))
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  print(noquote(sprintf("theta = %f", rOptions$theta)))
  print(noquote(sprintf("tieBias = %f", rOptions$tieBias)))
  print(noquote(sprintf("tieBeta = %f", rOptions$tieBeta)))
}
