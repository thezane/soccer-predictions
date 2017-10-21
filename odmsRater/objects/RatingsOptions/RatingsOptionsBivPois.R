new.RatingsOptionsBivPois <- function() {
  rOptions <- new.RatingsOptions()
  rOptions$iterName <- "odms-iter-bivpois"
  rOptions$writeName <- "odms-matches-bivpois"
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptionsBivPois(rOptions)

  class(rOptions) <- c("RatingsOptionsBivPois", class(rOptions))
  rOptions
}

constructLayersComputer.RatingsOptionsBivPois <- function(rOptions) {
  computeLayers <- function(rOptions, game) {
	meanGoals <- computeLayerHa(game, rOptions)
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoals)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)

    if (game$isRelevant || rOptions$isOptimized) {
      gamePrediction <- computeLayerBivPois(game, rOptions, meanGoals)
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

getModel.RatingsOptionsBivPois <- function(rOptions) {
  c(rOptions$meanGoals, rOptions$haBias,
      rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$strBeta, rOptions$corrBeta)
}

getModelLBd.RatingsOptionsBivPois <- function(rOptions) {
  c(rOptions$meanGoalsLBd, rOptions$haBiasLBd,
      rOptions$bLBd, rOptions$cLBd,
      rOptions$kLBd,
      rOptions$strBetaLBd, rOptions$corrBetaLBd)
}

getModelUBd.RatingsOptionsBivPois <- function(rOptions) {
  c(rOptions$meanGoalsUBd, rOptions$haBiasUBd,
      rOptions$bUBd, rOptions$cUBd,
      rOptions$kUBd,
      rOptions$strBetaUBd, rOptions$corrBetaUBd)
}

getSlopes.RatingsOptionsBivPois <- function(rOptions) {
  matrix(c(rOptions$haBias,
      rOptions$c,
      rOptions$strBeta))
}

update.RatingsOptionsBivPois <- function(rOptions, x) {
  rOptions$meanGoals <- x[1]
  rOptions$haBias <- x[2]
  rOptions$b <- x[3]
  rOptions$c <- x[4]
  rOptions$k <- x[5]
  rOptions$strBeta <- x[6]
  rOptions$corrBeta <- x[7]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg * t(slopes) %*% slopes
  rOptions
}

print.RatingsOptionsBivPois <- function(rOptions) {
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("haBias = %f", rOptions$haBias)))
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
  print(noquote(sprintf("corr = %f", rOptions$corrBeta)))
}
