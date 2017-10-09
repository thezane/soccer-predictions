new.RatingsOptionsBivPoisOdmMod <- function() {
  rOptions <- new.RatingsOptionsBivPois()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-bivpois-odmmod"
  rOptions$writeName <- "odms-matches-bivpois-odmmod"
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptionsBivPois(rOptions)

  class(rOptions) <- c("RatingsOptionsBivPoisOdmMod", c(rOptions))
  rOptions
}

getModel.RatingsOptionsBivPoisOdmMod <- function(rOptions) {
  getModel.RatingsOptionsBivPois(rOptions)
}

getModelLBd.RatingsOptionsBivPoisOdmMod <- function(rOptions) {
  getModelLBd.RatingsOptionsBivPois(rOptions)
}

getModelUBd.RatingsOptionsBivPoisOdmMod <- function(rOptions) {
  getModelUBd.RatingsOptionsBivPois(rOptions)
}

getSlopes.RatingsOptionsBivPoisOdmMod <- function(rOptions) {
  getSlopes.RatingsOptionsBivPois(rOptions)
}

update.RatingsOptionsBivPoisOdmMod <- function(rOptions, x) {
  update.RatingsOptionsBivPois(rOptions, x)
}

print.RatingsOptionsBivPoisOdmMod <- function(rOptions) {
  print.RatingsOptionsBivPois(rOptions)
}
