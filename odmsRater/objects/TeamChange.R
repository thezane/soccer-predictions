newTeamChange <- function(T, i) {
  date <- T[[i, "Date"]]
  type <- T[[i, "Type"]]
  change <- T[[i, "Change"]]

  teamChange <- list(
    date=date,
    type=type,
    change=change
  )

  class(teamChange) <- "TeamChange"
  teamChange
}
