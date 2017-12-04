new.RatingsOptionsSkellamDiff <- function() {
  rOptions <- new.RatingsOptionsSkellam()
  rOptions$iterName <- "odms-iter-skellam-diff"
  rOptions$writeName <- "odms-matches-skellam-diff"
  rOptions$layersComputer <- computeLayers.RatingsOptionsSkellamDiff

  class(rOptions) <- c("RatingsOptionsSkellamDiff", c(class(rOptions)))
  rOptions
}

getModel.RatingsOptionsSkellamDiff <- function(rOptions) {
  getModel.RatingsOptionsSkellam(rOptions)
}

getModelLBd.RatingsOptionsSkellamDiff <- function(rOptions) {
  getModelLBd.RatingsOptionsSkellam(rOptions)
}

getModelUBd.RatingsOptionsSkellamDiff <- function(rOptions) {
  getModelUBd.RatingsOptionsSkellam(rOptions)
}

getSlopes.RatingsOptionsSkellamDiff <- function(rOptions) {
  getSlopes.RatingsOptionsSkellam(rOptions)
}

update.RatingsOptionsSkellamDiff <- function(rOptions, x) {
  update.RatingsOptionsSkellam(rOptions, x)
}

print.RatingsOptionsSkellamDiff <- function(rOptions) {
  print.RatingsOptionsSkellam(rOptions)
}
