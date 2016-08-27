newHomeAdvantage <- function() {
  hA <- list(
      qHomeGoals=0,
      qAwayGoals=0,
      numQsHA=0,
      tHomeGoals=0,
      tAwayGoals=0,
      numTsHA=0,
      tNeutralGoals=0,
      numTsNeutral=0,
      hAData=NULL
  )
  class(hA) <- "HomeAdvantage"
  hA
} 

updateHA <- function(hA, game) {
  goals <- game$goals
  
  if (game$isQualifier) {
    hA$qHomeGoals <- hA$qHomeGoals + goals[1]
    hA$qAwayGoals <- hA$qAwayGoals + goals[2]
    hA$numQsHA <- hA$numQsHA + 1;
  }
  else if (game$existsHA) {
    hA$tHomeGoals <- hA$tHomeGoals + goals[1];
    hA$tAwayGoals <- hA$tAwayGoals + goals[2];
    hA$numTsHA <- hA$numTsHA + 1;
  }
  else {
    hA$tNeutralGoals <- hA$tNeutralGoals + sum(goals);
    hA$numTsNeutral <- hA$numTsNeutral + 1;
  }
  
  hA
}

updateHAData <- function(hA) {
  qHomeMeanGoals <- hA$qHomeGoals / hA$numQsHA
  qAwayMeanGoals <- hA$qAwayGoals / hA$numQsHA
  qHA <- qHomeMeanGoals / qAwayMeanGoals
  tAwayGoals <- hA$tAwayGoals + hA$tNeutralGoals
  numTsAway <- hA$numTsHA + 2 * hA$numTsNeutral
  tHomeMeanGoals <- hA$tHomeGoals / hA$numTsHA
  tAwayMeanGoals <- tAwayGoals / numTsAway
  tHA <- tHomeMeanGoals / tAwayMeanGoals
  hA$hAData <- list(
      qHomeMeanGoals=qHomeMeanGoals,
      qAwayMeanGoals=qAwayMeanGoals,
      tHomeMeanGoals=tHomeMeanGoals,
      tAwayMeanGoals=tAwayMeanGoals,
      qHA=qHA,
      tHA=tHA)
  hA
}
