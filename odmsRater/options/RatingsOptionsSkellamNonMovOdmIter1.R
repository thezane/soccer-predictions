new.RatingsOptionsSkellamNonMovOdmIter1 <- function() {
  rOptions <- new.RatingsOptionsSkellamNonMov()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-skellam-nonmov-odmiter1"
  rOptions$writeName <- "odms-matches-skellam-nonmov-odmiter1"

  class(rOptions) <- c("RatingsOptionsSkellamNonMovOdmIter1",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSkellamNonMovOdmIter1 <- function(rOptions) {
  getModel.RatingsOptionsSkellamNonMov(rOptions)
}

getModelLBd.RatingsOptionsSkellamNonMovOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptionsSkellamNonMov(rOptions)
}

getModelUBd.RatingsOptionsSkellamNonMovOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptionsSkellamNonMov(rOptions)
}

getSlopes.RatingsOptionsSkellamNonMovOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptionsSkellamNonMov(rOptions)
}

update.RatingsOptionsSkellamNonMovOdmIter1 <- function(rOptions, x) {
  update.RatingsOptionsSkellamNonMov(rOptions, x)
}

print.RatingsOptionsSkellamNonMovOdmIter1 <- function(rOptions) {
  print.RatingsOptionsSkellamNonMov(rOptions)
}
