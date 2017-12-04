new.RatingsOptionsSkellamNonMov <- function() {
  rOptions <- new.RatingsOptionsSkellam()
  rOptions$iterName <- "odms-iter-skellam-nonmov"
  rOptions$writeName <- "odms-matches-skellam-nonmov"
  rOptions$layersComputer <- computeLayers.RatingsOptionsSkellamNonMov

  class(rOptions) <- c("RatingsOptionsSkellamNonMov", class(rOptions))
  rOptions
}

getModel.RatingsOptionsSkellamNonMov <- function(rOptions) {
  getModel.RatingsOptionsSkellam(rOptions)
}

getModelLBd.RatingsOptionsSkellamNonMov <- function(rOptions) {
  getModelLBd.RatingsOptionsSkellam(rOptions)
}

getModelUBd.RatingsOptionsSkellamNonMov <- function(rOptions) {
  getModelUBd.RatingsOptionsSkellam(rOptions)
}

getSlopes.RatingsOptionsSkellamNonMov <- function(rOptions) {
  getSlopes.RatingsOptionsSkellam(rOptions)
}

update.RatingsOptionsSkellamNonMov <- function(rOptions, x) {
  update.RatingsOptionsSkellam(rOptions, x)
}

print.RatingsOptionsSkellamNonMov <- function(rOptions) {
  print.RatingsOptionsSkellam(rOptions)
}
