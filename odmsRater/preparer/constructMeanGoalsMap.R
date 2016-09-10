constructMeanGoalsMap <- function(T) {
  meanGoalsData <- constructMeanGoalsData(T)
  model <- rlm(Goals~GeneralContest, meanGoalsData)
  meanGoalsMap <- hash()
  modelBetas <- model$coefficients
  beta0 <- modelBetas[["(Intercept)"]]
  meanGoalsMap[["-Q-Away"]] <- beta0
  contestBetas <- names(modelBetas) 
  n <- length(contestBetas)
  i <- 2

  while (i <= n) {
    contestBeta <- contestBetas[i]
    meanGoalsMap[sub("GeneralContest", "", contestBeta)] <-
        beta0 + modelBetas[[contestBeta]]
    i <- i + 1
  }

  meanGoalsMap
}

constructMeanGoalsData <- function(T) {
  n <- nrow(T)
  meanGoalsData <- data.frame(matrix(nrow=2*n, ncol=0))
  meanGoalsData[1: n, "Contest"] <- T["Contest"]
  meanGoalsData[1: n, "Goals"] <- T["HomeGoals"]
  meanGoalsData[1: n, "HomeAdvantage"] <- T["HomeAdvantage"]
  meanGoalsData[(n + 1): (2 * n), "Contest"] <- T["Contest"]
  meanGoalsData[(n + 1): (2 * n), "Goals"] <- T["AwayGoals"]
  meanGoalsData[(n + 1): (2 * n), "HomeAdvantage"] <- 0
  meanGoalsData[, "GeneralContest"] <- apply(meanGoalsData, 1,
      getGeneralContest)
  meanGoalsData
}

getGeneralContest <- function(meanGoalsRow) {
  contest <- meanGoalsRow[["Contest"]]
  
  if (grepl("-Q", contest)) {
    contestSubbed <- "-Q"
  }
  else {
    contestSubbed <- "-T"
  }
  
  if (as.logical(as.numeric(meanGoalsRow[["HomeAdvantage"]]))) {
    paste(contestSubbed, "Home", sep="-")
  }
  else {
    paste(contestSubbed, "Away", sep="-")
  }
}
