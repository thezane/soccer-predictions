getGeneralContest <- function(tContest) {
  if (grepl("Q", tContest)) {
    generalContest <- "qualifier"
  }
  else if (grepl("G", tContest)) {
    generalContest <- "group"
  }
  else {
    generalContest <- "knockout"
  }
  
  generalContest
} 
