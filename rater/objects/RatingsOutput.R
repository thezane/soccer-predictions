newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    strCost=0,
    strMedianCosts=NULL,
    y=Inf
  )
  class(rOutput) <- "RatingsOutput"
  rOutput
}

updateStrMedianCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  teamStrNorms <- computeStrNorm(data.frame(teams[["teamStr"]]))
  strNormMedian <- c(median(teamStrNorms[[1]]),
      median(teamStrNorms[[2]]))
  strMedianCost <- computeMSE(strNormMedian, c(0, 0))
  rOutput$strMedianCosts <- c(rOutput$strMedianCosts, strMedianCost)
  rOutput
}

computeStrMedianCost <- function(rOutput) {
  strMedianCosts <- rOutput$strMedianCosts

  if (is.null(strMedianCosts)) {
    strMedianCost <- 0
  }
  else {
    strMedianCost <- mean(strMedianCosts)
  }

  strMedianCost
}