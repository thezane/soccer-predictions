library(ggplot2)

fileNames <- c("output/HomeWin", "output/Tie", "output/AwayWin")
inFileExtension <- ".csv"
outFileExtension <- ".png"

for (fileName in fileNames) {
  inName <- paste(fileName, inFileExtension, sep="")
  outName <- paste(fileName, outFileExtension, sep="")
  predictions = read.csv(inName, header=FALSE)
  sigmas <- apply(predictions, 1, function(row) {
      sqrt(sum((row - mean(row)) ^ 2) / length(row))})
  histogram <- qplot(sigmas, geom="histogram") 
  ggsave(histogram, file=outName)
}
