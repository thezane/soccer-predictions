new.RatingsOptionsSkellamGeomDiff <- function() {
  rOptions <- new.RatingsOptionsSkellam()
  rOptions$r <- 0.5
  rOptions$iterName <- "odms-iter-skellam-geomdiff"
  rOptions$writeName <- "odms-matches-skellam-geomdiff"
  rOptions$layersComputer <-
      computeLayers.RatingsOptionsSkellamGeomDiff

  class(rOptions) <- c("RatingsOptionsSkellamGeomDiff",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  getModel.RatingsOptionsSkellam(rOptions)
}

getModelLBd.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  getModelLBd.RatingsOptionsSkellam(rOptions)
}

getModelUBd.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  getModelUBd.RatingsOptionsSkellam(rOptions)
}

getSlopes.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  getSlopes.RatingsOptionsSkellam(rOptions)
}

update.RatingsOptionsSkellamGeomDiff <- function(rOptions, x) {
  update.RatingsOptionsSkellam(rOptions, x)
}

print.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  print.RatingsOptionsSkellam(rOptions)
}
