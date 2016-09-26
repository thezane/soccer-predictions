getQualifierLocation <- function(contest) {
  if (contest == "EUC-Q") {
    location <- "Europe"
  }
  else {
    regex <- "WOC-Q-((\\w|\\s)+)"
    location <- sub(regex, "\\1", contest[grepl(regex, contest)])
  }
  
  location
} 
