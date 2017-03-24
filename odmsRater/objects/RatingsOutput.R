newRatingsOutput <- function(tTree, gTree, gi) {
  rOutput <- list(
    tTree=tTree,
    gTree=gTree,
    gi=gi,
    strCosts=NULL,
    goalsCosts=NULL,
    strMeanCosts=NULL,
    oceaniaResults=NULL,
    oceaniaGoals=NULL
  )
  
  class(rOutput) <- "RatingsOutput"
  rOutput
}

# Update cost of prediction.
updateOceania <- function(rOutput, t, resultExpected, resultActual, goalsExpected, goalsActual) {
  if (t) {
      rOutput$oceaniaResults = c(rOutput$oceaniaResults, resultExpected - resultActual)
      rOutput$oceaniaGoals = c(rOutput$oceaniaGoals, goalsExpected - goalsActual)
  }

  rOutput
}

# Update cost of prediction.
updateStrCost <- function(rOutput, resultExpected, resultActual) {
  rOutput$strCosts = c(rOutput$strCosts, resultExpected - resultActual)
  rOutput
}

# Update distance of expected goals from actual goals.
updateGoalsCost <- function(rOutput, goalsExpected, goalsActual) {
  rOutput$goalsCosts <- c(rOutput$goalsCosts,
      goalsExpected - goalsActual)
  rOutput
}

# Update distance of mean team rating from default rating.
updateStrMeanCosts <- function(rOutput) {
  teams <- data.frame(t(values(rOutput$tTree)))
  strNorms <- data.frame(teams[["strNorm"]])
  strNormMean <- c(mean(strNorms[[1]]), mean(strNorms[[2]]))
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
    strCost <- mean(abs(strCosts))
  }

  strCost
}

# Compute distance of expected goals from actual goals.
computeGoalsCost <- function(rOutput) {
  goalsCosts <- rOutput$goalsCost

  if (is.null(goalsCosts)) {
    goalsCost <- 0
  }
  else {
    goalsCost <- mean(abs(goalsCosts))
  }

  goalsCost
}

# Compute distance of mean team rating from default rating.
computeStrMeanCost <- function(rOutput) {
  strMeanCosts <- rOutput$strMeanCosts

  if (is.null(strMeanCosts)) {
    strMeanCost <- 0
  }
  else {
	strMeanCost <- mean(abs(strMeanCosts))
  }

  strMeanCost
}
