new.RatingsOptionsOdmMod <- function() {
  rOptions <- new.RatingsOptions()
  rOptions$odmIter <- 1
  rOptions$iterName <- "odms-iter-odmmod"
  rOptions$writeName <- "odms-matches-odmmod"
  
  class(rOptions) <- c("RatingsOptionsOdmMod", class(rOptions))
  rOptions
}

getModel.RatingsOptionsOdmMod <- function(rOptions) {
  getModel.RatingsOptions(rOptions)
}


getModelLBd.RatingsOptionsOdmMod <- function(rOptions) {
  getModelLBd.RatingsOptions(rOptions)
}


getModelUBd.RatingsOptionsOdmMod <- function(rOptions) {
  getModelUBd.RatingsOptions(rOptions)
}


getSlopes.RatingsOptionsOdmMod <- function(rOptions) {
  getSlopes.RatingsOptions(rOptions)
}


update.RatingsOptionsOdmMod <- function(rOptions, x) {
  update.RatingsOptions(rOptions, x)
}

print.RatingsOptionsOdmMod <- function(rOptions) {
  print.RatingsOptions(rOptions)
}
