verifyModel <- function(model, currentYear) {
  dataPath <- "../accuracy/"
  fileName <- paste(dataPath, "verified", model, currentYear, ".csv",
      sep ="")
  model <- read.csv(fileName, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  verifiedPredictions <- apply(model, 1, verifyPrediction)
  model["MSE"] <- verifiedPredictions
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
  mse <- round(sum((expectedResult - actualResult) ^ 2), numDecimals)
  mse
}
