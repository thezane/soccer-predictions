new.RatingsOptions <- function() {
  rOptions <- list(
    # ODM layer
    b=0.3,
    c=0.3,

    # Ratings layer
    k=0.16,

    # Bivariate Poisson layer
    meanGoals=1,
    strBeta=1.6,
    corrBeta=-1,
    
    # Poisson layer
    theta=1,
    
    # Mixture layer
    tieBias=-3.2,
    tieBeta=-4,  

    # Lower bounds for optimizable parameters
    bLBd=0.01,
    cLBd=0.01,
    kLBd=0,
    meanGoalsLBd=0.01,
    strBetaLBd=0.01,
    corrBetaLBd=-Inf,
    thetaLBd=0.01,
    tieBiasLBd=-Inf,
    tieBetaLBd=-Inf,
    
    # Upper bounds for optimizable parameters
    bUBd=Inf,
    cUBd=Inf,
    kUBd=1,
    meanGoalsUBd=Inf,
    strBetaUBd=Inf,
    corrBetaUBd=0,
    thetaUBd=Inf,
    tieBiasUBd=Inf,
    tieBetaUBd=0,
    
    # Non-optimizable paramters
    fTree=list(
        "Africa"=c(0.04, -0.04),
        "Asia"=c(-0.28, 0.28),
        "Europe"=c(0.32, -0.32),
        "North America"=c(-0.2, 0.2),
        "Oceania"=c(-0.56, 0.56),
        "South America"=c(0.52, -0.52)),
    wTree=list(
        "very high"=1,
        "high"=5/6,
        "medium"=4/6,
        "low"=3/6,
        "very low"=2/6
    ),
    dateFormat="%Y-%m-%d",
    isOptimized=FALSE,
    iterName="odms-iter",
    hA=0.32,
    minUpdatesUntilReliable=10,
    odmIter=10,
    pGoalsMatSize=20,
    writeName="odms-matches",
    
    # Regularization
    slopeCost=0,
    strMeanCostReg=0.1,
    slopeCostReg=0.01,

    # L-BFGS-B parameters
    factr=1e-04 / .Machine$double.eps,
    lmm=10
  )

  rOptions$currentDate <- as.Date("1994-07-18", rOptions$dateFormat)
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptions(rOptions)

  class(rOptions) <- "RatingsOptions"
  rOptions
}

constructLayersComputer.RatingsOptions <- function(rOptions) {
  computeLayers <- function(rOptions, game) {
    strPostNorm <- computeLayerOdm(game, rOptions)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)

    if (game$isRelevant) {
      gamePredictionBivPois <- computeLayerBivPois(game, rOptions)
      gamePredictionPois <- computeLayerPois(game, rOptions)
      gamePrediction <- computeLayerMixture(game,
          gamePredictionBivPois, gamePredictionPois, rOptions)
    }
    else {
      gamePrediction = NULL
    }

    layerOutput <- list(gamePrediction=gamePrediction,
        strNextNorm=strNextNorm)
    layerOutput
  }

  computeLayers
}

getModel <- function(class) {
  UseMethod("getModel")
}

getModel.RatingsOptions <- function(rOptions) {
  c(rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$meanGoals, rOptions$strBeta,
      rOptions$corrBeta,
      rOptions$theta, rOptions$tieBias, rOptions$tieBeta)
}

getModelLBd <- function(class) {
  UseMethod("getModelLBd")
}

getModelLBd.RatingsOptions <- function(rOptions) {
  c(rOptions$bLBd, rOptions$cLBd,
      rOptions$kLBd,
      rOptions$meanGoalsLBd, rOptions$strBetaLBd,
      rOptions$corrBetaLBd,
      rOptions$thetaLBd, rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

getModelUBd <- function(class) {
  UseMethod("getModelUBd")
}

getModelUBd.RatingsOptions <- function(rOptions) {
  c(rOptions$bUBd, rOptions$cUBd,
      rOptions$kUBd,
      rOptions$meanGoalsUBd, rOptions$strBetaUBd,
      rOptions$corrBetaUBd,
      rOptions$thetaUBd, rOptions$tieBiasUBd, rOptions$tieBetaUBd)
}

getSlopes <- function(class) {
  UseMethod("getSlopes")
}

getSlopes.RatingsOptions <- function(rOptions) {
  0
}

update <- function(class, x) {
  UseMethod("update")
}

update.RatingsOptions <- function(rOptions, x) {
  rOptions$b <- x[1]
  rOptions$c <- x[2]
  rOptions$k <- x[3]
  rOptions$meanGoals <- x[4]
  rOptions$strBeta <- x[5]
  rOptions$corrBeta <- x[6]
  rOptions$theta <- x[7]
  rOptions$tieBias <- x[8]
  rOptions$tieBeta <- x[9]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg *
      (t(slopes) %*% slopes) / length(slopes)
  rOptions
}

print.RatingsOptions <- function(rOptions) {
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  print(noquote(sprintf("theta = %f", rOptions$theta)))
  print(noquote(sprintf("tieBias = %f", rOptions$tieBias)))
  print(noquote(sprintf("tieBeta = %f", rOptions$tieBeta)))
}
