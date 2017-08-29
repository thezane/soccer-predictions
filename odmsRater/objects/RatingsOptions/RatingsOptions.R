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
    hA=0.32,
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
    hALBd=0,
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
    hAUBd=Inf,
    corrBetaUBd=0,
    thetaUBd=Inf,
    tieBias=Inf,
    tieBeta=-1,
    
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
        "moderate"=4/6,
        "low"=3/6,
        "very low"=2/6
    ),
    dateFormat="%m/%d/%y",
    isOptimized=FALSE,
    iterName="odms-iter.csv",
    minUpdatesUntilReliable=10,
    odmIter=10,
    pGoalsMatSize=20,
    writeName="odms-matches.csv",
    
    # Regularization
    slopeCost=0,
    strMeanCostReg=0.1,
    slopeCostReg=0.001,

    # L-BFGS-B parameters
    factr=1e-04 / .Machine$double.eps,
    lmm=10
  )

  rOptions$currentDate <- as.Date("6/11/14", rOptions$dateFormat)
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

getModel.RatingsOptions <- function(rOptions) {
  c(rOptions$b, rOptions$c,
    rOptions$k,
    rOptions$meanGoals, rOptions$strBeta, rOptions$hA,
        rOptions$corrBeta,
        rOptions$theta, rOptions$tieBias, rOptions$tieBeta)
}

getModelLBd.RatingsOptions <- function(rOptions) {
  c(rOptions$bLBd, rOptions$cLBd,
    rOptions$kLBd,
    rOptions$meanGoalsLBd, rOptions$strBetaLBd, rOptions$hALBd,
        rOptions$corrBetaLBd,
        rOptions$thetaLBd, rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

getModelUBd.RatingsOptions <- function(rOptions) {
  c(rOptions$bUBd, rOptions$cUBd,
    rOptions$kUBd,
    rOptions$meanGoalsUBd, rOptions$strBetaUBd, rOptions$hAUBd,
        rOptions$corrBetaUBd,
        rOptions$thetaUBd, rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

update.RatingsOptions <- function(rOptions, x) {
  rOptions$b <- x[1]
  rOptions$c <- x[2]
  rOptions$k <- x[3]
  rOptions$meanGoals <- x[4]
  rOptions$strBeta <- x[5]
  rOptions$hA <- x[6]
  rOptions$corrBeta <- x[7]
  rOptions$theta <- x[8]
  rOptions$tieBias <- x[9]
  rOptions$tieBeta <- x[10]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$slopeCost <- rOptions$slopeCostReg *
      norm(matrix(c(rOptions$b, rOptions$strBeta, rOptions$hA)), "f")
  rOptions
}

print.RatingsOptions <- function(rOptions) {
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
  print(noquote(sprintf("theta = %f", rOptions$theta)))
  print(noquote(sprintf("tieBias = %f", rOptions$tieBias)))
  print(noquote(sprintf("tieBeta = %f", rOptions$tieBeta)))
}
