constructContests <- function(inputPath) {
  cTree <- hash()
  contestSrc <- paste(inputPath, "contests.csv", sep="")
  T <- read.csv(contestSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- nrow(T)
  i <- 1

  while (i <= n) {
    contestData <- list()
    contestData[["relevance"]] <- T[[i, "Relevance"]]
    contestData[["weight"]] <- T[[i, "Weight"]]
    cTree[[T[[i, "Contest"]]]] <- contestData
    i <- i + 1
  }

  cTree
}
