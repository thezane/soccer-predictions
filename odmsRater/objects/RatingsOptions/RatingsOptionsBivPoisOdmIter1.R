new.RatingsOptionsBivPoisOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsBivPois()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-bivpois-odmiter1"
  rOptions$writeName <- "odms-matches-bivpois-odmiter1"
  rOptions$layersComputer <-
      constructLayersComputer.RatingsOptionsBivPois(rOptions)

  class(rOptions) <- c("RatingsOptionsBivPoisOdmIter1", c(rOptions))
  rOptions
}

getModel.RatingsOptionsBivPoisOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsBivPois(rOptions)
}

getModelLBd.RatingsOptionsBivPoisOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsBivPois(rOptions)
}

getModelUBd.RatingsOptionsBivPoisOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsBivPois(rOptions)
}

getSlopes.RatingsOptionsBivPoisOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsBivPois(rOptions)
}

update.RatingsOptionsBivPoisOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsBivPois(rOptions, x)
}

print.RatingsOptionsBivPoisOdmIter1 <- function(rOptions) {
  print.RatingsOptionsBivPois(rOptions)
}
