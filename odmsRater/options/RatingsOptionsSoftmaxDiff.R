new.RatingsOptionsSoftmaxDiff <- function() {
  rOptions <- new.RatingsOptionsSoftmax()
  rOptions$iterName <- "odms-iter-softmax-diff"
  rOptions$writeName <- "odms-matches-softmax-diff"
  rOptions$layersComputer <- computeLayers.RatingsOptionsSoftmaxDiff

  class(rOptions) <- c("RatingsOptionsSoftmaxDiff", class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxDiff <- function(rOptions) {
  getModel.RatingsOptionsSoftmax(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxDiff <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmax(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxDiff <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmax(rOptions)
}

getSlopes.RatingsOptionsSoftmaxDiff <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmax(rOptions)
}

update.RatingsOptionsSoftmaxDiff <- function(rOptions, x) {
  update.RatingsOptionsSoftmax(rOptions, x)
}

print.RatingsOptionsSoftmaxDiff <- function(rOptions) {
  print.RatingsOptionsSoftmax(rOptions)
}
