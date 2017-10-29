new.RatingsOptionsSoftmaxNonMovOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSoftmaxNonMov()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-softmax-nonmov-odmiter1"
  rOptions$writeName <- "odms-matches-softmax-nonmov-odmiter1"

  class(rOptions) <- c("RatingsOptionsSoftmaxNonMovOdmIter1",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxNonMovOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSoftmaxNonMov(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxNonMovOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmaxNonMov(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxNonMovOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmaxNonMov(rOptions)
}

getSlopes.RatingsOptionsSoftmaxNonMovOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmaxNonMov(rOptions)
}

update.RatingsOptionsSoftmaxNonMovOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSoftmaxNonMov(rOptions, x)
}

print.RatingsOptionsSoftmaxNonMovOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSoftmaxNonMov(rOptions)
}
