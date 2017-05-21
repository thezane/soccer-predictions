newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    strCosts=NULL,
    strMeanCosts=NULL
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

# Update cost of prediction.
updateStrCost <- function(rOutput, p) {
  rOutput$strCosts <- c(rOutput$strCosts, p)
  rOutput
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  strNorms <- data.frame(teams[["strNorm"]])
  aNorms <- unlist(strNorms[1, ])
  dNorms <- unlist(strNorms[2, ])
  strNormMean <- c(mean(aNorms), mean(dNorms)) /
      c(sd(aNorms), sd(dNorms))
  rOutput$strMeanCosts <- c(rOutput$strMeanCosts,
      strNormMean - c(0, 0))
  rOutput
}

# Compute cost of prediction.
computeStrCost <- function(rOutput) {
  strCosts <- rOutput$strCosts

  if (is.null(strCosts)) {
    strCost <- 0
  }
  else {
    strCost <- mean(-log(strCosts))
  }

  strCost
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost <- function(rOutput) {
  strMeanCosts <- rOutput$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- 0
  }
  else {
	strMeanCost <- mean(strMeanCosts ^ 2)
  }

  strMeanCost
}
