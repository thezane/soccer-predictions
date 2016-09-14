constructMeanGoalsMap <- function(T, tTree) {
  meanGoalsData <- constructMeanGoalsData(T, tTree)
  model <- rlm(Goals~GeneralContest, meanGoalsData, psi=psi.bisquare)
  meanGoalsMap <- hash()
  modelBetas <- model$coefficients
  beta0 <- modelBetas[["(Intercept)"]]
  meanGoalsMap[["-Q-Away-Africa"]] <- beta0
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

constructMeanGoalsData <- function(T, tTree) {
  n <- nrow(T)
  meanGoalsData <- data.frame(matrix(nrow=2*n, ncol=0))
  meanGoalsData[1: n, "Team"] <- T["HomeTeam"]
  meanGoalsData[1: n, "Contest"] <- T["Contest"]
  meanGoalsData[1: n, "Goals"] <- T["HomeGoals"]
  meanGoalsData[1: n, "HomeAdvantage"] <- T["HomeAdvantage"]
  meanGoalsData[(n + 1): (2 * n), "Team"] <- T["AwayTeam"]
  meanGoalsData[(n + 1): (2 * n), "Contest"] <- T["Contest"]
  meanGoalsData[(n + 1): (2 * n), "Goals"] <- T["AwayGoals"]
  meanGoalsData[(n + 1): (2 * n), "HomeAdvantage"] <- 0
  meanGoalsData[, "GeneralContest"] <- apply(meanGoalsData, 1,
      function (meanGoalsRow, tTree.=tTree) {
	      getGeneralContest(meanGoalsRow, tTree)
	  })
  meanGoalsData
}

getGeneralContest <- function(meanGoalsRow, tTree) {
  contest <- meanGoalsRow[["Contest"]]
  team <- tTree[[meanGoalsRow[["Team"]]]]
  isQualifier <- grepl("-Q", contest)
  isPlayOff=grepl("-P", contest)
  isHome <- as.logical(as.numeric(meanGoalsRow[["HomeAdvantage"]]))

  if (isQualifier && !isPlayOff && isHome) {
    generalContest <- paste("-Q-Home-", team$fName, sep="")
  }
  else if (isQualifier && !isPlayOff) {
    generalContest <- paste("-Q-Away-", team$fName, sep="")
  }
  else if (isHome) {
    generalContest <- "-T-Home"
  }
  else {
    generalContest <- "-T-Away"
  }

  generalContest
}
