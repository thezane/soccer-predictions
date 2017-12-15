library(ggplot2)

fileNames <- c("output/HomeWin", "output/Tie", "output/AwayWin")
inFileExtension <- ".csv"
outFileExtension <- ".png"

for (fileName in fileNames) {
  inName <- paste(fileName, inFileExtension, sep="")
  outName <- paste(fileName, outFileExtension, sep="")
  predictions = read.csv(inName, header=TRUE)
  sigmas <- apply(predictions, 1, sd)
  histogram <- qplot(sigmas, geom="histogram") 
  ggsave(histogram, file=outName)
}
