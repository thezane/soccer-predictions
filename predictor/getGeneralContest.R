getGeneralContest <- function(tContest) {
  if (grepl("-Q", tContest)) {
    generalContest <- "q"
  }
  else {
    generalContest <- "t"
  }
  
  generalContest
} 
