new.RatingsOptionsSkellamOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSkellam()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-skellam-odmiter1"
  rOptions$writeName <- "odms-matches-skellam-odmiter1"

  class(rOptions) <- c("RatingsOptionsSkellamOdmIter1",
      c(class(rOptions)))
  rOptions
}

getModel.RatingsOptionsSkellamOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSkellam(rOptions)
}

getModelLBd.RatingsOptionsSkellamOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsSkellam(rOptions)
}

getModelUBd.RatingsOptionsSkellamOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsSkellam(rOptions)
}

getSlopes.RatingsOptionsSkellamOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSkellam(rOptions)
}

update.RatingsOptionsSkellamOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSkellam(rOptions, x)
}

print.RatingsOptionsSkellamOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSkellam(rOptions)
}
