source("getOutputs.R")

computeSds <- function(outputs, inputNames, numGames) {
  predictionNames <- c(
      inputNames[["homeWinName"]],
      inputNames[["tieName"]],
      inputNames[["awayWinName"]])
  stdevs <- data.frame(matrix(ncol=length(predictionNames), nrow=0),
      stringsAsFactors=FALSE)
  colnames(stdevs) <- predictionNames
  i <- 1

  while (i < numGames) {
    row <- list()

    for (predictionName in predictionNames) {
      output <- outputs[[predictionName]]
      stdev <- sd(unlist(output[i, ]))     
      row[predictionName] <- stdev
    }

    stdevs[i, ] <- row
    print(paste("Compute sd of game ", i, sep=""))
    i <- i + 1
  }

  stdevs
}

computeSds2 <- function(outputs, inputNames, numGames) {
  stdevs <- c()
  gameInfo <- outputs[[inputNames[["gameInfoName"]]]]
  predictionNames <- c(
      inputNames[["homeWinName"]],
      inputNames[["tieName"]],
      inputNames[["awayWinName"]])
  i <- 1

  while (i < numGames) {
    predictions <- list()

    for (predictionName in predictionNames) {
      output <- outputs[[predictionName]]
      prediction <- unlist(output[i, ])  
      predictions[[predictionName]] <- prediction
    }

    homePrediction <- mean(predictions[[inputNames[["homeWinName"]]]])
    awayPrediction <- mean(predictions[[inputNames[["awayWinName"]]]])

    if (homePrediction > awayPrediction) {
      stdev <- sd(predictions[[inputNames[["awayWinName"]]]])
    }
    else {
      stdev <- sd(predictions[[inputNames[["homeWinName"]]]])
    }

    stdevs <- c(stdevs, stdev)
    print(paste("Compute sd of weaker team of game ", i, sep=""))
    i <- i + 1
  }

  stdevs
}

computeHardAccuracies <- function(outputs, inputNames, numGames) {
  hardAccuracies <- c()
  gameInfo <- outputs[[inputNames[["gameInfoName"]]]]
  predictionNames <- c(
      inputNames[["homeWinName"]],
      inputNames[["tieName"]],
      inputNames[["awayWinName"]])
  i <- 1

  while (i < numGames) {
    predictions <- list()

    for (predictionName in predictionNames) {
      output <- outputs[[predictionName]]
      prediction <- unlist(output[i, ])  
      predictions[[predictionName]] <- prediction
    }

    homeGoals <- gameInfo[[i, "HomeGoals"]]
    awayGoals <- gameInfo[[i, "AwayGoals"]]

    if (homeGoals > awayGoals) {
      correctPredictionName <- inputNames[["homeWinName"]]
    }
    else if (homeGoals == awayGoals) {
      correctPredictionName <- inputNames[["tieName"]]
    }
    else {
      correctPredictionName <- inputNames[["awayWinName"]]
    }

    wrongPredictionNames <- c()

    for (predictionName in predictionNames) {
      if (predictionName != correctPredictionName) {
        wrongPredictionNames <- c(wrongPredictionNames, predictionName)
      }
    }

    hardAccuracy <- mean(
        (predictions[[correctPredictionName]] >
        predictions[[wrongPredictionNames[1]]]) &
        (predictions[[correctPredictionName]] >
        predictions[[wrongPredictionNames[2]]]))
    hardAccuracies <- c(hardAccuracies, hardAccuracy)
    print(paste("Compute hard accuracy of game ", i, sep=""))
    i <- i + 1
  }

  hardAccuracies
}

computeSoftAccuracies <- function(outputs, inputNames, numGames) {
  softAccuracies <- c()
  gameInfo <- outputs[[inputNames[["gameInfoName"]]]]
  i <- 1

  while (i < numGames) {
    homeGoals <- gameInfo[[i, "HomeGoals"]]
    awayGoals <- gameInfo[[i, "AwayGoals"]]

    if (homeGoals > awayGoals) {
      inputName <- "homeWinName"
    }
    else if (homeGoals == awayGoals) {
      inputName <- "tieName"
    }
    else {
      inputName <- "awayWinName"
    }

    output <- outputs[[inputNames[[inputName]]]]
    softAccuracy <- mean(unlist(output[i, ]))
    softAccuracies <- c(softAccuracies, softAccuracy)
    print(paste("Compute soft accuracy of game ", i, sep=""))
    i <- i + 1
  }

  softAccuracies
}

outputFolder <- "output"
inputNames <- list(
    gameInfoName="GameInfo.csv",
    homeWinName="HomeWin.csv",
    tieName="Tie.csv",
    awayWinName="AwayWin.csv")
outputs <- getOutputs(inputNames, outputFolder)
gameInfo <- outputs[[inputNames[["gameInfoName"]]]]
numGames <- nrow(gameInfo)
stdevs <- computeSds(outputs, inputNames, numGames)
stdevs2 <- computeSds2(outputs, inputNames, numGames)
softAccuracies <- computeSoftAccuracies(outputs, inputNames, numGames)
hardAccuracies <- computeHardAccuracies(outputs, inputNames, numGames)
