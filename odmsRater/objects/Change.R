newChange <- function(T, i) {
  name <- T[[i, "Team"]]
  date <- T[[i, "Date"]]
  type <- T[[i, "Type"]]
  change <- T[[i, "Change"]]

  change <- list(
    name=name,
    date=date,
    type=type,
    change=change
  )

  class(change) <- "Change"
  change
}

handleChange <- function(change, tTree) {
  if (change$type == "Creation") {
    tTree <- handleCreation(change, tTree)
  }
  else if (change$type == "Destruction") {
    tTree <- handleDestruction(change, tTree)
  }
  else if (change$type == "Federation") {
    tTree <- handleFederation(change, tTree)
  }
  else if (change$type == "Name") {
    tTree <- handleName(change, tTree)
  }

  tTree
}

handleCreation <- function(change, tTree) {
  teamName <- change$name
  fName <- change$change
  tTree[[teamName]] <- newTeam(teamName, fName)
  tTree
}

handleDestruction <- function(change, tTree) {
  teamName <- change$name
  tTree[[teamName]] <- NULL
  tTree
}

handleFederation <- function(change, tTree) {
  teamName <- change$name
  fNameNew <- change$change
  team <- tTree[[teamName]]
  team$fName <- fNameNew
  team$numUpdates <- 0
  tTree[[teamName]] <- team
  tTree
}

handleName <- function(change, tTree) {
  teamName <- change$name
  teamNameNew <- change$change
  tTree[[teamNameNew]] <- tTree[[teamName]]
  tTree[[teamNameNew]]$name <- teamNameNew
  tTree[[teamName]] <- NULL 
  tTree
}
