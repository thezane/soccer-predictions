constructChanges <- function(rOptions, inputPath) {
  gTree <- hash()
  changesSrc <- paste(inputPath, "changes.csv", sep="")
  dateFormat <- rOptions$dateFormat
  T <- read.csv(changesSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- nrow(T)
  i <- 1

  while (i <= n) {
    changeDate <- as.Date(T[[i, "Date"]], dateFormat)
    gTree <- addChange(T, i, gTree, changeDate)
    i <- i + 1
  }

  gTree
}

addChange <- function(T, i, gTree, changeDate) {
  changeDateStr <- as.character(changeDate)

  if (!has.key(changeDateStr, gTree)) {
    gTree[[changeDateStr]] <- NULL
  }

  gDateList <- gTree[[changeDateStr]]
  gDateList[[length(gDateList) + 1]] <- new.Change(T, i)
  gTree[[changeDateStr]] <- gDateList
  gTree
}
