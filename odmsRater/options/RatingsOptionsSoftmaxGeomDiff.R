new.RatingsOptionsSoftmaxGeomDiff <- function() {
  rOptions <- new.RatingsOptionsSoftmax()
  rOptions$r <- 0.5
  rOptions$iterName <- "odms-iter-softmax-geomdiff"
  rOptions$writeName <- "odms-matches-softmax-geomdiff"
  rOptions$layersComputer <-
      computeLayers.RatingsOptionsSoftmaxGeomDiff

  class(rOptions) <- c("RatingsOptionsSoftmaxGeomDiff",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxGeomDiff <- function(rOptions) {
  getModel.RatingsOptionsSoftmax(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxGeomDiff <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmax(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxGeomDiff <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmax(rOptions)
}

getSlopes.RatingsOptionsSoftmaxGeomDiff <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmax(rOptions)
}

update.RatingsOptionsSoftmaxGeomDiff <- function(rOptions, x) {
  update.RatingsOptionsSoftmax(rOptions, x)
}

print.RatingsOptionsSoftmaxGeomDiff <- function(rOptions) {
  print.RatingsOptionsSoftmax(rOptions)
}
