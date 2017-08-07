newTeamChange <- function(T, i) {
  name <- T[[i, "Team"]]
  date <- T[[i, "Date"]]
  type <- T[[i, "Type"]]
  change <- T[[i, "Change"]]

  teamChange <- list(
    name=name,
    date=date,
    type=type,
    change=change
  )

  class(teamChange) <- "TeamChange"
  teamChange
}

handleChange <- function(teamChange, tTree) {
  if (teamChange$type == "Creation") {
    tTree <- handleCreation(teamChange, tTree)
  }
  else if (teamChange$type == "Destruction") {
    tTree <- handleDestruction(teamChange, tTree)
  }
  else if (teamChange$type == "Federation") {
    tTree <- handleFederation(teamChange, tTree)
  }
  else if (teamChange$type == "Name") {
    tTree <- handleName(teamChange, tTree)
  }

  tTree
}

handleCreation <- function(teamChange, tTree) {
  teamName <- teamChange$name
  fName <- teamChange$change
  tTree[[teamName]] <- newTeam(teamName, fName)
  tTree
}

handleDestruction <- function(teamChange, tTree) {
  teamName <- teamChange$name
  tTree[[teamName]] <- NULL
  tTree
}

handleFederation <- function(teamChange, tTree) {
  teamName <- teamChange$name
  fNameNew <- teamChange$change
  team <- tTree[[teamName]]
  team$fName <- fNameNew
  team$numUpdates <- 0
  tTree[[teamName]] <- team
  tTree
}

handleName <- function(teamChange, tTree) {
  teamName <- teamChange$name
  teamNameNew <- teamChange$change
  tTree[[teamNameNew]] <- tTree[[teamName]]
  tTree[[teamNameNew]]$name <- teamNameNew
  tTree[[teamName]] <- NULL 
  tTree
}
