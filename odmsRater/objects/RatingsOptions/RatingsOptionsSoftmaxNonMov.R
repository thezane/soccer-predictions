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
	gamePrediction <- NULL
    strNextNorm <- NULL
	meanGoals <- computeLayerHa(game, rOptions)
	
	if (game$computeRatings) {
	  game <- computeLayerGoalsNonMov(game)
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
