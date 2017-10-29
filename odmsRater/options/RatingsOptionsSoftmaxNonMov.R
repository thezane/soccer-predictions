new.RatingsOptionsSoftmaxNonMov <- function() {
  rOptions <- new.RatingsOptionsSoftmax()
  rOptions$iterName <- "odms-iter-softmax-nonmov"
  rOptions$writeName <- "odms-matches-softmax-nonmov"
  rOptions$layersComputer <- computeLayers.RatingsOptionsSoftmaxNonMov

  class(rOptions) <- c("RatingsOptionsSoftmaxNonMov", class(rOptions))
  rOptions
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
