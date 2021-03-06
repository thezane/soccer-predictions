verifyModel <- function(model, currentContest, currentYear) {
  library(nnet)
  dataPath <- "../../accuracy/"
  fileName <- paste(dataPath, "verified", model,
      currentContest, currentYear, ".csv", sep ="")
  model <- read.csv(fileName, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  verifiedPredictions <- do.call(rbind.data.frame,
      apply(model, 1, verifyPrediction))
  model["SSE"] <- verifiedPredictions[["SSE"]]
  model["IsCorrect"] <- verifiedPredictions[["IsCorrect"]]
  write.csv(model, fileName, row.names=FALSE)
} 

verifyPrediction <- function(matchesRow) {
  numDecimals <- 4
  expectedResult <- as.numeric(
      c(matchesRow[["HomeWin"]], matchesRow[["Tie"]],
      matchesRow[["AwayWin"]]))
  homeGoals <- matchesRow[["HomeGoals"]]
  awayGoals <- matchesRow[["AwayGoals"]]
  actualResult <- c(homeGoals > awayGoals, homeGoals == awayGoals,
      homeGoals < awayGoals)
  isCorrect <- as.numeric(which.is.max(expectedResult) ==
      which.is.max(actualResult))
  sse <- round(sum((expectedResult - actualResult) ^ 2), numDecimals)
  verifiedPredictions <- list(IsCorrect=isCorrect, SSE=sse)
  verifiedPredictions
}
