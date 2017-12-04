new.RatingsOptionsSkellamGeomDiffOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSkellamGeomDiff()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-skellam-geomdiff-odmiter1"
  rOptions$writeName <- "odms-matches-skellam-geomdiff-odmiter1"

  class(rOptions) <- c("RatingsOptionsSkellamGeomDiffOdmIter1",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSkellamGeomDiffOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSkellamGeomDiff(rOptions)
}

getModelLBd.RatingsOptionsSkellamGeomDiffOdmIter1 <- function(
    rOptions) {
  getModelLBd.RatingsOptionsSkellamGeomDiff(rOptions)
}

getModelUBd.RatingsOptionsSkellamGeomDiffOdmIter1 <- function(
    rOptions) {
  getModelUBd.RatingsOptionsSkellamGeomDiff(rOptions)
}

getSlopes.RatingsOptionsSkellamGeomDiffOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSkellamGeomDiff(rOptions)
}

update.RatingsOptionsSkellamGeomDiffOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSkellamGeomDiff(rOptions, x)
}

print.RatingsOptionsSkellamGeomDiffOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSkellamGeomDiff(rOptions)
}
