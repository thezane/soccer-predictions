new.RatingsOptionsSoftmaxOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSoftmax()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-softmax-odmiter1"
  rOptions$writeName <- "odms-matches-softmax-odmiter1"
  
  class(rOptions) <- c("RatingsOptionsSoftmaxOdmIter1",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSoftmax(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmax(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmax(rOptions)
}

getSlopes.RatingsOptionsSoftmaxOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmax(rOptions)
}

update.RatingsOptionsSoftmaxOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSoftmax(rOptions, x)
}

print.RatingsOptionsSoftmaxOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSoftmax(rOptions)
}
