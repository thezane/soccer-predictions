constructTeamChanges <- function(gTree, dateFormat, dataPath) {
  teamChangesSrc <- paste(dataPath, "team-changes.csv", sep="")
  T <- read.csv(teamChangesSrc, header=TRUE, sep=",", quote="\"", 
      stringsAsFactors=FALSE)
  n <- nrow(T)
  i <- 1

  while (i <= n) {
    teamChangeDate <- as.Date(T[[i, "Date"]], dateFormat)
    gTree <- addTeamChange(T, i, gTree, teamChangeDate)
    i <- i + 1
  }

  gTree
}

addTeamChange <- function(T, i, gTree, teamChangeDate) {
  if (!has.key(gameDateStr, gTree)) {
    gTree[gameDateStr] <- NULL
  }

  gDateList <- gTree[[gameDateStr]]
  gDateList[length(gDateList) + 1] <- newTeamChange(T, i)
  gTree[[gameDateStr]] <- gDateList
  gTree
}
