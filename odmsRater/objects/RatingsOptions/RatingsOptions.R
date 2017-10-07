new.RatingsOptions <- function() {
  rOptions <- list(
    # Ha layer
    meanGoals=1,
    haBias=0.32,
    haBeta=0.32,
    timeBias=398.4,
    timeBeta=-0.2,

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
    tieBeta=-2,  

    # Lower bounds for optimizable parameters
    meanGoalsLBd=0.01,
    haBiasLBd=0.01,
    haBetaLBd=0.01,
    timeBiasLBd=0.01,
    timeBetaLBd=-Inf,
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
    haBetaUBd=Inf,
    timeBiasUBd=Inf,
    timeBetaUBd=0,
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

  rOptions$currentDate <- as.Date("2003-01-01", rOptions$dateFormat)
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptions(rOptions)

  class(rOptions) <- "RatingsOptions"
  rOptions
}

constructLayersComputer.RatingsOptions <- function(rOptions) {
  computeLayers <- function(rOptions, game) {
	meanGoalsData <- computeLayerHa(game, rOptions)
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoalsData)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)

    if (game$isRelevant) {
      gamePredictionBivPois <- computeLayerBivPois(game, rOptions,
          meanGoalsData)
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
  c(rOptions$meanGoals, rOptions$haBias, rOptions$haBeta,
      rOptions$timeBias, rOptions$timeBeta,
      rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$meanGoals, rOptions$strBeta,
      rOptions$corrBeta,
      rOptions$theta, rOptions$tieBias, rOptions$tieBeta)
}

getModelLBd <- function(class) {
  UseMethod("getModelLBd")
}

getModelLBd.RatingsOptions <- function(rOptions) {
  c(rOptions$meanGoalsLBd, rOptions$haBiasLBd, rOptions$haBetaLBd,
      rOptions$timeBiasLBd, rOptions$timeBetaLBd,
      rOptions$bLBd, rOptions$cLBd,
      rOptions$kLBd,
      rOptions$meanGoalsLBd, rOptions$strBetaLBd,
      rOptions$corrBetaLBd,
      rOptions$thetaLBd, rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

getModelUBd <- function(class) {
  UseMethod("getModelUBd")
}

getModelUBd.RatingsOptions <- function(rOptions) {
  c(rOptions$meanGoalsUBd, rOptions$haBiasUBd, rOptions$haBetaUBd,
      rOptions$timeBiasUBd, rOptions$timeBetaUBd,
      rOptions$bUBd, rOptions$cUBd,
      rOptions$kUBd,
      rOptions$meanGoalsUBd, rOptions$strBetaUBd,
      rOptions$corrBetaUBd,
      rOptions$thetaUBd, rOptions$tieBiasUBd, rOptions$tieBetaUBd)
}

getSlopes <- function(class) {
  UseMethod("getSlopes")
}

getSlopes.RatingsOptions <- function(rOptions) {
  c()
}

update <- function(class, x) {
  UseMethod("update")
}

update.RatingsOptions <- function(rOptions, x) {
  rOptions$meanGoals <- x[1]
  rOptions$haBias <- x[2]
  rOptions$haBeta <- x[3]
  rOptions$timeBias <- x[4]
  rOptions$timeBeta <- x[5]
  rOptions$b <- x[6]
  rOptions$c <- x[7]
  rOptions$k <- x[8]
  rOptions$meanGoals <- x[9]
  rOptions$strBeta <- x[10]
  rOptions$corrBeta <- x[11]
  rOptions$theta <- x[12]
  rOptions$tieBias <- x[13]
  rOptions$tieBeta <- x[14]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- 0
  rOptions
}

print.RatingsOptions <- function(rOptions) {
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("haBias = %f", rOptions$haBias)))
  print(noquote(sprintf("haBeta = %f", rOptions$haBeta)))
  print(noquote(sprintf("timeBias = %f", rOptions$timeBias)))
  print(noquote(sprintf("timeBeta = %f", rOptions$timeBeta)))
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
