new.RatingsOptionsSkellam <- function() {
  rOptions <- new.RatingsOptions()
  rOptions$pGoalsMatSize <- 20
  rOptions$iterName <- "odms-iter-skellam"
  rOptions$writeName <- "odms-matches-skellam"
  rOptions$layersComputer <- computeLayers.RatingsOptionsSkellam

  class(rOptions) <- c("RatingsOptionsSkellam", class(rOptions))
  rOptions
}

getModel.RatingsOptionsSkellam <- function(rOptions) {
  c(rOptions$meanGoals, rOptions$haBias,
      rOptions$b, rOptions$c,
      rOptions$k,
      rOptions$strBeta)
}

getModelLBd.RatingsOptionsSkellam <- function(rOptions) {
  c(rOptions$meanGoalsLBd, rOptions$haBiasLBd,
      rOptions$bLBd, rOptions$cLBd,
      rOptions$kLBd,
      rOptions$strBetaLBd)
}

getModelUBd.RatingsOptionsSkellam <- function(rOptions) {
  c(rOptions$meanGoalsUBd, rOptions$haBiasUBd,
      rOptions$bUBd, rOptions$cUBd,
      rOptions$kUBd,
      rOptions$strBetaUBd)
}

getSlopes.RatingsOptionsSkellam <- function(rOptions) {
  matrix(c(rOptions$haBias,
      rOptions$b,
      rOptions$strBeta))
}

update.RatingsOptionsSkellam <- function(rOptions, x) {
  rOptions$meanGoals <- x[1]
  rOptions$haBias <- x[2]
  rOptions$b <- x[3]
  rOptions$c <- x[4]
  rOptions$k <- x[5]
  rOptions$strBeta <- x[6]
  rOptions$strBetas <- c(rOptions$strBeta, -rOptions$strBeta)
  slopes <- getSlopes.RatingsOptions(rOptions)
  rOptions$slopeCost <- rOptions$slopeCostReg * t(slopes) %*% slopes
  rOptions
}

print.RatingsOptionsSkellam <- function(rOptions) {
  print(noquote(sprintf("mu = %f", rOptions$meanGoals)))
  print(noquote(sprintf("haBias = %f", rOptions$haBias)))
  print(noquote(sprintf("b = %f", rOptions$b)))
  print(noquote(sprintf("c = %f", rOptions$c)))
  print(noquote(sprintf("k = %f", rOptions$k)))
  print(noquote(sprintf("strBeta = %f", rOptions$strBeta)))
}
