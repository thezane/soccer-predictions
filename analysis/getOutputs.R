getOutputs <- function(outputNames, outputFolder) {
  outputs <- list()

  for (outputName in outputNames) {
    outputPath <- paste(outputFolder, "/", outputName, sep="")
    output <- read.csv(outputPath, header=TRUE, sep=",", quote="\"", 
        stringsAsFactors=FALSE)
    outputs[[outputName]] <- output
  }

  outputs
}
