new.RatingsOptionsSoftmaxNonMov <- function() {
  rOptions <- new.RatingsOptionsSoftmax()
  rOptions$iterName <- "odms-iter-softmax-nonmov"
  rOptions$writeName <- "odms-matches-softmax-nonmov"
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptionsSoftmaxNonMov(rOptions)

  class(rOptions) <- c("RatingsOptionsSoftmaxNonMov", class(rOptions))
  rOptions
}

constructLayersComputer.RatingsOptionsSoftmaxNonMov <- function(rOptions) {
  computeLayers <- function(rOptions, game) {
	game <- computeLayerGoalsNonMov(game)
	meanGoalsData <- computeLayerHa(game, rOptions)
    strPostNorm <- computeLayerOdm(game, rOptions, meanGoalsData)
    strNextNorm <- computeLayerRatings(game, rOptions, strPostNorm)

    if (game$isRelevant || rOptions$isOptimized) {
      gamePrediction <- computeLayerSoftmax(game, rOptions,
          meanGoalsData)
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

getModel.RatingsOptionsSoftmaxNonMov <- function(rOptions) {
  getModel.RatingsOptionsSoftmax(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxNonMov <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmax(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxNonMov <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmax(rOptions)
}

getSlopes.RatingsOptionsSoftmaxNonMov <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmax(rOptions)
}

update.RatingsOptionsSoftmaxNonMov <- function(rOptions, x) {
  update.RatingsOptionsSoftmax(rOptions, x)
}

print.RatingsOptionsSoftmaxNonMov <- function(rOptions) {
  print.RatingsOptionsSoftmax(rOptions)
}
