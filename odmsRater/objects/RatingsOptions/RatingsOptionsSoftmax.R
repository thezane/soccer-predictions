new.RatingsOptionsSoftmax <- function() {
  rOptions <- new.RatingsOptions()
  rOptions$tieBias <- 0.4
  rOptions$tieBeta <- -1.0
  rOptions$slopeCostReg <- 0.01
  rOptions$iterName <- "odms-iter-softmax"
  rOptions$writeName <- "odms-matches-softmax"
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptionsSoftmax(rOptions)

  class(rOptions) <- c("RatingsOptionsSoftmax", class(rOptions))
  rOptions
}

constructLayersComputer.RatingsOptionsSoftmax <- function(rOptions) {
  computeLayers <- function(rOptions, game) {
	gamePrediction <- NULL
    strNextNorm <- NULL
	meanGoals <- computeLayerHa(game, rOptions)
	
	if (game$computeRatings) {
      strPostNorm <- computeLayerOdm(game, rOptions, meanGoals)
      strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)
    }

    if (game$isRelevant || rOptions$isOptimized) {
      gamePrediction <- computeLayerSoftmax(game, rOptions, meanGoals)
    }

    layerOutput <- list(gamePrediction=gamePrediction,
        strNextNorm=strNextNorm)
    layerOutput
  }

  computeLayers
}

getModel.RatingsOptionsSoftmax <- function(rOptions) {
  c(rOptions$meanGoals, rOptions$haBias,
      rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$strBeta,
      rOptions$tieBias, rOptions$tieBeta)
}

getModelLBd.RatingsOptionsSoftmax <- function(rOptions) {
  c(rOptions$meanGoalsLBd, rOptions$haBiasLBd,
    rOptions$bLBd, rOptions$cLBd,
    rOptions$kLBd,
    rOptions$strBetaLBd,
    rOptions$tieBiasLBd, rOptions$tieBetaLBd)
}

getModelUBd.RatingsOptionsSoftmax <- function(rOptions) {
  c(rOptions$meanGoalsUBd, rOptions$haBiasUBd,
    rOptions$bUBd, rOptions$cUBd,
    rOptions$kUBd,
    rOptions$strBetaUBd,
    rOptions$tieBiasUBd, rOptions$tieBetaUBd)
}

getSlopes.RatingsOptionsSoftmax <- function(rOptions) {
  matrix(c(rOptions$haBias,
      rOptions$c,
      rOptions$strBeta,
      rOptions$tieBeta))
}

update.RatingsOptionsSoftmax <- function(rOptions, x) {
  rOptions$meanGoals <- x[1]
  rOptions$haBias <- x[2]
  rOptions$b <- x[3]
  rOptions$c <- x[4]
  rOptions$k <- x[5]
  rOptions$strBeta <- x[6]
  rOptions$tieBias <- x[7]
  rOptions$tieBeta <- x[8]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg *
      (t(slopes) %*% slopes) / length(slopes)
  rOptions
}

print.RatingsOptionsSoftmax <- function(rOptions) {
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("haBias = %f", rOptions$haBias)))
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("tieBias = %f", rOptions$tieBias)))
  print(noquote(sprintf("tieBeta = %f", rOptions$tieBeta)))
}
