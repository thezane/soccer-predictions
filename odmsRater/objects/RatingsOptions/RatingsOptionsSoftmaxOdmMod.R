new.RatingsOptionsSoftmaxOdmMod <- function() {
  rOptions <- list(
    # ODM layer
    b=0.3,
    c=0.3,

    # Ratings layer
    k=0.16,

    # Softmax layer
    meanGoals=1,
    strBeta=1.6,
    hA=0.32,
    tieBias=0,
    tieBeta=-2,  

    # Lower bounds for optimizable parameters
    bLBd=0.01,
    cLBd=0.01,
    kLBd=0,
    meanGoalsLBd=0.01,
    strBetaLBd=0.01,
    hALBd=0,
    tieBiasLBd=-Inf,
    tieBetaLBd=-Inf,
    
    # Upper bounds for optimizable parameters
    bUBd=Inf,
    cUBd=Inf,
    kUBd=1,
    meanGoalsUBd=Inf,
    strBetaUBd=Inf,
    hAUBd=Inf,
    tieBiasUBd=Inf,
    tieBetaUBd=Inf,
    
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
    iterName="odms-iter-softmax-odmmod.csv",
    minUpdatesUntilReliable=10,
    odmIter=1,
    pGoalsMatSize=20,
    writeName="odms-matches-softmax-odmmod.csv",
    
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
      constructLayersComputer.RatingsOptionsSoftmaxOdmMod(rOptions)

  class(rOptions) <- c("RatingsOptionsSoftmax", "RatingsOptions")
  rOptions
}

constructLayersComputer.RatingsOptionsSoftmaxOdmMod <- function(
    rOptions) {
  computeLayers <- function(rOptions, game) {
    strPostNorm <- computeLayerOdm(game, rOptions)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)

    if (game$isRelevant) {
      gamePrediction <- computeLayerSoftmax(game, rOptions)
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

getModel.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  c(rOptions$b, rOptions$c,
    rOptions$k,
    rOptions$meanGoals, rOptions$strBeta, rOptions$hA,
        rOptions$tieBias, rOptions$tieBeta)
}

getModelLBd.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  c(rOptions$bLBd, rOptions$cLBd,
    rOptions$kLBd,
    rOptions$meanGoalsLBd, rOptions$strBetaLBd, rOptions$hALBd,
        rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

getModelUBd.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  c(rOptions$bUBd, rOptions$cUBd,
    rOptions$kUBd,
    rOptions$meanGoalsUBd, rOptions$strBetaUBd, rOptions$hAUBd,
        rOptions$tieBiasUBd, rOptions$tieBetaUBd)
}

getSlopes.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  c(rOptions$b,
      rOptions$strBeta, rOptions$hA,
      rOptions$tieBias)
}

update.RatingsOptionsSoftmaxOdmMod <- function(rOptions, x) {
  rOptions$b <- x[1]
  rOptions$c <- x[2]
  rOptions$k <- x[3]
  rOptions$meanGoals <- x[4]
  rOptions$strBeta <- x[5]
  rOptions$hA <- x[6]
  rOptions$tieBias <- x[7]
  rOptions$tieBeta <- x[8]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg *
      (t(slopes) %*% slopes) / length(slopes)
  rOptions
}

print.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("ha = %f", rOptions$hA)))
  print(noquote(sprintf("tieBias = %f", rOptions$tieBias)))
  print(noquote(sprintf("tieBeta = %f", rOptions$tieBeta)))
}
