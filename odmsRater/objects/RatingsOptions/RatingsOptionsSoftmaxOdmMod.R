new.RatingsOptionsSoftmaxOdmMod <- function() {
  rOptions <- new.RatingsOptionsSoftmax()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-softmax-odmmod"
  rOptions$writeName <- "odms-matches-softmax-odmmod"
  
  class(rOptions) <- c("RatingsOptionsSoftmaxOdmMod", class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  getModel.RatingsOptionsSoftmax(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  getModelLBd.RatingsOptionsSoftmax(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  getModelUBd.RatingsOptionsSoftmax(rOptions)
}

getSlopes.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmax(rOptions)
}

update.RatingsOptionsSoftmaxOdmMod <- function(rOptions, x) {
  update.RatingsOptionsSoftmax(rOptions, x)
}

print.RatingsOptionsSoftmaxOdmMod <- function(rOptions) {
  print.RatingsOptionsSoftmax(rOptions)
}
