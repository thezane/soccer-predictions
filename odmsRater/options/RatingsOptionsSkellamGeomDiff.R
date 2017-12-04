new.RatingsOptionsSkellamGeomDiff <- function() {
  rOptions <- new.RatingsOptionsSkellam()
  
  # Goals layer
  rOptions$r <- 0.5
  
  # Lower bounds for optimizable parameters
  rOptions$rLBd <- 0.0
  
  # Upper bounds for optimizable parameters
  rOptions$rUBd <- 0.99
  
  rOptions$iterName <- "odms-iter-skellam-geomdiff"
  rOptions$writeName <- "odms-matches-skellam-geomdiff"
  rOptions$layersComputer <-
      computeLayers.RatingsOptionsSkellamGeomDiff

  class(rOptions) <- c("RatingsOptionsSkellamGeomDiff",
      class(rOptions))
  rOptions
}

getModel.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  c(rOptions$meanGoals, rOptions$haBias,
      rOptions$r,
      rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$strBeta)
}

getModelLBd.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  c(rOptions$meanGoalsLBd, rOptions$haBiasLBd,
    rOptions$rLBd,
    rOptions$bLBd, rOptions$cLBd,
    rOptions$kLBd,
    rOptions$strBetaLBd)
}

getModelUBd.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  c(rOptions$meanGoalsUBd, rOptions$haBiasUBd,
    rOptions$rUBd,
    rOptions$bUBd, rOptions$cUBd,
    rOptions$kUBd,
    rOptions$strBetaUBd)
}

getSlopes.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  getSlopes.RatingsOptionsSkellam(rOptions)
}

update.RatingsOptionsSkellamGeomDiff <- function(rOptions, x) {
  rOptions$meanGoals <- x[1]
  rOptions$haBias <- x[2]
  rOptions$r <- x[3]
  rOptions$b <- x[4]
  rOptions$c <- x[5]
  rOptions$k <- x[6]
  rOptions$strBeta <- x[7]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg *
      (t(slopes) %*% slopes) / length(slopes)
  rOptions
}

print.RatingsOptionsSkellamGeomDiff <- function(rOptions) {
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("haBias = %f", rOptions$haBias)))
  print(noquote(sprintf("r = %f", rOptions$r)))
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
}
