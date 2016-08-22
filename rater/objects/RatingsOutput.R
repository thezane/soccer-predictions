newRatingsOutput <- function(tTree, gTree, gi, currentDate) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    currentDate=currentDate,
    strCost=0,
    strMeanCosts=NULL,
    y=Inf
  )
  class(rOutput) <- "RatingsOutput"
  rOutput
}

updateStrMeanCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  teamStrNorms <- computeStrNorm(data.frame(teams[["teamStr"]]))
  strNormMean <- c(mean(teamStrNorms[[1]]),
      mean(teamStrNorms[[2]]))
  strMeanCost <- c(computeMSE(strNormMean[1], 0),
      computeMSE(strNormMean[2], 0))
  rOutput$strMeanCosts <- c(rOutput$strMeanCosts, strMeanCost)
  rOutput
}

computeStrMeanCost <- function(rOutput) {
  strMeanCosts <- rOutput$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- c(0, 0)
  }
  else {
	strMeanCostsMat <- matrix(strMeanCosts, ncol=2, byrow=TRUE)
    strMeanCost <- c(mean(strMeanCostsMat[, 1]),
        mean(strMeanCostsMat[, 2]))
  }

  strMeanCost
}
