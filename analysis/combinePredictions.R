computeGameValidity <- function(outputs, i) {
  for (output in outputs) {
    if (output[i, "HomeReliability"] < 1 ||
        output[i, "AwayReliability"] < 1) {
      return(FALSE)
    }
  }

  return(TRUE)
}

getOutputs <- function(outputNames) {
  outputs <- list()

  for (outputName in outputNames) {
    outputPath <- paste(outputFolder, "/", outputName, sep="")
    output <- read.csv(outputPath, header=TRUE, sep=",", quote="\"", 
        stringsAsFactors=FALSE)
    outputs[[outputName]] <- output
  }

  outputs
}

writeGameInfo <- function(outputs, outputFolder, outputFirst,
    columnsGameInfo, numColumnsGameInfo, numGames) {
  tableGameInfo <- data.frame(matrix(ncol=numColumnsGameInfo, nrow=0),
      stringsAsFactors=FALSE)
  colnames(tableGameInfo) <- columnsGameInfo
  validGames <- c()
  currentRowNum <- 1
  i <- 1

  while (i < numGames) {
    row <- list()
    isGameValid <- computeGameValidity(outputs, i)
    validGames <- c(validGames, isGameValid)

    if (validGames[i]) {
      row <- outputFirst[i, columnsGameInfo]
      tableGameInfo[currentRowNum, ] <- row
      print(paste("Add game ", i, " to info table", sep=""))
      currentRowNum <- currentRowNum + 1
    }

    i <- i + 1
  }

  write.csv(tableGameInfo, paste(outputFolder, "/GameInfo.csv", sep=""),
      row.names=FALSE)
  validGames
}

writeGamePredictions <- function(outputs, outputFolder, outputNames,
    numGames, numOutputs, validGames, prediction) {
  tableGamePrediction <- data.frame(
      matrix(ncol=numOutputs, nrow=0), stringsAsFactors=FALSE)
  colnames(tableGamePrediction) <- outputNames
  currentRowNum <- 1
  i <- 1

  while (i < numGames) {
    if (validGames[i]) {
      row <- list()
      j <- 1

      while (j < numOutputs) {
        outputName <- outputNames[j]
        output <- outputs[[j]]
        row[outputName] <- output[[i, prediction]]
        j <- j + 1
      }

      tableGamePrediction[currentRowNum, ] <- row
      print(paste("Add game ", i, " to ", prediction, " table", sep=""))
      currentRowNum <- currentRowNum + 1
    }

    i <- i + 1
  }

  write.csv(tableGamePrediction,
      paste(outputFolder, "/", prediction, ".csv", sep=""), row.names=FALSE)
}

outputFolder <- "../output"
analysisOutputFolder <- "output"
fileNames <- list.files(outputFolder)
fileNameMatches <- grepl("^odms-matches.*\\.*csv$", fileNames, perl=TRUE)
outputNamesExt <- fileNames[fileNameMatches]
outputNames <- gsub("\\.csv$", "", outputNamesExt)
outputs <- getOutputs(outputNamesExt)
numOutputs <- length(outputs)
outputFirst <- outputs[[outputNamesExt[1]]]
numGames <- nrow(outputFirst)
columnsGameInfo <- c(
    "HomeTeam",
    "AwayTeam",
    "Date",
    "Contest",
    "HomeGoals",
    "AwayGoals",
    "HomeAdvantage")
numColumnsGameInfo <- length(columnsGameInfo)
validGames <- writeGameInfo(outputs, analysisOutputFolder, outputFirst,
    columnsGameInfo, numColumnsGameInfo, numGames)
columnsGamePredictions <- c(
    "HomeWin",
    "Tie",
    "AwayWin")
for (columnGamePredictions in columnsGamePredictions) {
  writeGamePredictions(outputs, analysisOutputFolder, outputNames,
      numGames, numOutputs, validGames, columnGamePredictions)
}
