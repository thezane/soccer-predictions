new.RatingsOptionsOdmIter1 <- function() {
  rOptions <- new.RatingsOptions()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-odmiter1"
  rOptions$writeName <- "odms-matches-odmiter1"
  
  class(rOptions) <- c("RatingsOptionsOdmIter1", class(rOptions))
  rOptions
}

getModel.RatingsOptionsOdmIter1 <- function(rOptions) {
  getModel.RatingsOptions(rOptions)
}


getModelLBd.RatingsOptionsOdmIter1 <- function(rOptions) {
  getModelLBd.RatingsOptions(rOptions)
}


getModelUBd.RatingsOptionsOdmIter1 <- function(rOptions) {
  getModelUBd.RatingsOptions(rOptions)
}


getSlopes.RatingsOptionsOdmIter1 <- function(rOptions) {
  getSlopes.RatingsOptions(rOptions)
}


update.RatingsOptionsOdmIter1 <- function(rOptions, x) {
  update.RatingsOptions(rOptions, x)
}

print.RatingsOptionsOdmIter1 <- function(rOptions) {
  print.RatingsOptions(rOptions)
}
