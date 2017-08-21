new.EventIterator <- function(gTree) {
  dates <- keys(gTree)
  gi <- list(
    gTree=gTree,
    dates=dates[order(dates)],
    I=c(0, 1),
    N=c(length(dates), 0)
  )
  class(gi) <- "EventIterator"
  gi
}

hasNext.EventIterator <- function(gi) {
  tf <- (gi$I[1] < gi$N[1]) || (gi$I[2] <= gi$N[2])
  tf
}

next.EventIterator <- function(gi) {
  if (gi$I[2] > gi$N[2]) {
    gi$I[1] <- gi$I[1] + 1
    gi$I[2] <- 1
    eventDate <- gi$dates[gi$I[1]]
    gi$N[2] <- length(gi$gTree[[eventDate]])
  }
               
  nextDate <- gi$dates[gi$I[1]]
  gDateList <- gi$gTree[[nextDate]]
  event <- gDateList[[gi$I[2]]]
  gi$I[2] <- gi$I[2] + 1
  eventData <- list(gi=gi, event=event)
  eventData
}

reset.EventIterator <- function(gi) {
  gi$I <- c(0, 1)
  gi$N <- c(length(gi$dates), 0)
  gi
}
