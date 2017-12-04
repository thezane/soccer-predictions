new.RatingsOptionsSkellamDiffOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSkellamDiff()
  rOptions$iterName <- "odms-iter-skellam-diff-odmiter1"
  rOptions$writeName <- "odms-matches-skellam-diff-odmiter1"

  class(rOptions) <- c("RatingsOptionsSkellamDiffOdmIter1",
      c(class(rOptions)))
  rOptions
}

getModel.RatingsOptionsSkellamDiffOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSkellam(rOptions)
}

getModelLBd.RatingsOptionsSkellamDiffOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsSkellam(rOptions)
}

getModelUBd.RatingsOptionsSkellamDiffOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsSkellam(rOptions)
}

getSlopes.RatingsOptionsSkellamDiffOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSkellam(rOptions)
}

update.RatingsOptionsSkellamDiffOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSkellam(rOptions, x)
}

print.RatingsOptionsSkellamDiffOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSkellam(rOptions)
}
