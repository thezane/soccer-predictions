new.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSoftmaxGeomDiff()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-softmax-geomdiff-odmiter1"
  rOptions$writeName <- "odms-matches-softmax-geomdiff-odmiter1"

  class(rOptions) <- c("RatingsOptionsSoftmaxGeomDiffOdmIter1",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSoftmaxGeomDiff(rOptions)
}

getModelLBd.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function(
    rOptions) {
  getModelLBd.RatingsOptionsSoftmaxGeomDiff(rOptions)
}

getModelUBd.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function(
    rOptions) {
  getModelUBd.RatingsOptionsSoftmaxGeomDiff(rOptions)
}

getSlopes.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSoftmaxGeomDiff(rOptions)
}

update.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSoftmaxGeomDiff(rOptions, x)
}

print.RatingsOptionsSoftmaxGeomDiffOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSoftmaxGeomDiff(rOptions)
}
