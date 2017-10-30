new.RatingsOptionsSoftmaxDiffOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSoftmaxDiff()
  rOptions$iterName <- "odms-iter-softmax-diff-odmiter1"
  rOptions$writeName <- "odms-matches-softmax-diff-odmiter1"

  class(rOptions) <- c("RatingsOptionsSoftmaxDiffOdmIter1",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxDiffOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSoftmax(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxDiffOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmax(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxDiffOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmax(rOptions)
}

getSlopes.RatingsOptionsSoftmaxDiffOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmax(rOptions)
}

update.RatingsOptionsSoftmaxDiffOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSoftmax(rOptions, x)
}

print.RatingsOptionsSoftmaxDiffOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSoftmax(rOptions)
}
