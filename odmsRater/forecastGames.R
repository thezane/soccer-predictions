forecastGames <- function(rData) {
  library(MASS)
  library(hash)
  library(parallel)
  library(skellam)
  regexRFiles <- ".*\\.R"
  inputPath <- "../input/"
  outputPath <- "../output/"
  srcFiles <- list.files(".", regexRFiles,
      full.names=TRUE, recursive=TRUE)
  sapply(srcFiles, source)
  rOptions <- rData$rOptions
  rOutput <- rData$rOutput
  dateFormat <- rOptions$dateFormat
  tTree <- rOutput$tTree
  matchSrc <- paste(inputPath, "fixtures.csv", sep="")
  T <- read.csv(matchSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  T <- T[order(as.Date(T[["Date"]], format=dateFormat)), ]
  colNames = c("HomeStrAgg", "AwayStrAgg",
      "HomeStrAtk", "HomeStrDef", "AwayStrAtk", "AwayStrDef",
      "HomeWin", "Tie", "AwayWin")
  T[colNames] <- 0
  n <- nrow(T)
  i <- 1
  
  while (i <= n) {
    homeTeamName <- T[[i, "HomeTeam"]]
    awayTeamName <- T[[i, "AwayTeam"]]
    existsHa <- T[[i, "HomeAdvantage"]]
    homeTeam <- tTree[[homeTeamName]]
    awayTeam <- tTree[[awayTeamName]]
    game <- new.GameHypo(homeTeamName, awayTeamName, existsHa, rData)
    layersOutput <- rOptions$layersComputer(rOptions, game)
    gamePrediction <- layersOutput[["gamePrediction"]]
    pWinTieLose <- gamePrediction[["pWinTieLose"]]
    T[i, "HomeStrAgg"] <- homeTeam$strAgg
    T[i, "AwayStrAgg"] <- awayTeam$strAgg
    T[i, "HomeStrAtk"] <- homeTeam$strNormBeta[1]
    T[i, "HomeStrDef"] <- homeTeam$strNormBeta[2]
    T[i, "AwayStrAtk"] <- awayTeam$strNormBeta[1]
    T[i, "AwayStrDef"] <- awayTeam$strNormBeta[2]
    T[i, "HomeWin"] <- pWinTieLose[1]
    T[i, "Tie"] <- pWinTieLose[2]
    T[i, "AwayWin"] <- pWinTieLose[3]
    i <- i + 1
  }

  outFile <- paste(outputPath, rOptions$writeName, "-fixtures", ".csv", sep="")
  write.csv(T, outFile, row.names=FALSE)
}
