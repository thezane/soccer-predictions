verifyModel <- function(model, currentYear) {
  library(nnet)
  dataPath <- "../../accuracy/"
  fileName <- paste(dataPath, "verified", model, currentYear, ".csv",
      sep ="")
  model <- read.csv(fileName, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  verifiedPredictions <- do.call(rbind.data.frame,
      apply(model, 1, verifyPrediction))
  model["MSE"] <- verifiedPredictions[["MSE"]]
  model["IsCorrect"] <- verifiedPredictions[["IsCorrect"]]
  write.csv(model, fileName, row.names=FALSE)
} 

verifyPrediction <- function(matchesRow) {
  numDecimals <- 4
  expectedResult <- as.numeric(
      c(matchesRow[["HomeWin"]], matchesRow[["Tie"]],
      matchesRow[["AwayWin"]]))
  actualResult <- c(1, 0, 0)
  homeGoals <- matchesRow[["HomeGoals"]]
  awayGoals <- matchesRow[["AwayGoals"]]
  actualResult <- c(homeGoals > awayGoals, homeGoals == awayGoals,
      homeGoals < awayGoals)
  isCorrect <- as.numeric(which.is.max(expectedResult) ==
      which.is.max(actualResult))
  mse <- round(sum((expectedResult - actualResult) ^ 2), numDecimals)
  verifiedPredictions <- list(IsCorrect=isCorrect, MSE=mse)
  verifiedPredictions
}
