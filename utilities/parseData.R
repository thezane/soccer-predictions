parseData <- function(filename) {
  library(hash)
  library(stringr)
  fp <- file(filename, "r")
  currentContest = NULL
  h <- constructTeamHash()
  rowNames <- c("HomeTeam", "AwayTeam", "Date", "Contest",
      "HomeGoals", "AwayGoals", "HomeAdvantage")
  data <- data.frame(row.names = rowNames)
  date = NA
  lines <- readLines(fp, )
  n <- length(lines)
  i <- 1
  j <- 1

  while (i <= n) {
    line <- lines[i]
    #contest <- getContest(line)
    dateTry <- getDateFormatted(line)
    
    if (!is.na(dateTry)) {
      date <- dateTry
    }
    
    goals <- getGoals(line)
    teams <- getTeams(line, h)

    #if (!is.na(contest)) {
    #  currentContest <- contest
    #}

    if (!is.na(date) && !is.na(goals) && !is.na(teams)) {
      data[j, "HomeTeam"] <- teams[1]
      data[j, "AwayTeam"] <- teams[2]
      data[j, "Date"] <- date
      data[j, "Contest"] <- "CAF-G"
      data[j, "HomeGoals"] <- goals[1]
      data[j, "AwayGoals"] <- goals[2]
      data[j, "HomeAdvantage"] <- 0
      j <- j + 1
    }

    i <- i + 1
  }

  close(fp)  
  write.csv(data, file = "data.csv", row.names = FALSE)
  h
}

getContest <- function(line) {
  fs <- c("Africa", "Asia", "Europe", "North America",
      "Oceania", "South America")

  if (line == "North and Central America") {
    line = "North America"
  }

  if (line %in% fs) {
    paste("WOC-Q-", line, sep="")
  }
  else {
    NA
  }
}

getDateFormatted <- function(line) {
  pattern <- "[0-9]+ [A-Za-z]+ [0-9]+"
  match <- str_match(line, pattern)
  date <- as.Date(match, "%d %B %Y")
  dateFormatted <- format(date, "%m/%d/%y")
  dateFormatted
}

getGoals <- function(line) {
  pattern <- "^[A-Za-z. ]+[a-z]  ([0-9]+)-([0-9]+)  [A-Za-z. ]+[a-z]$"
  matches <- str_match(line, pattern)

  if (!is.na(matches)) {
    c(as.numeric(matches[2]), as.numeric(matches[3]))
  }
  else {
    NA
  }
}

getTeams <- function(line, h) {
  pattern <- "^([A-Za-z. ]+[a-z])  [0-9]+-[0-9]+  ([A-Za-z. ]+[a-z])$"
  matches <- str_match(line, pattern)

  if (!is.na(matches)) {
    c(normTeams(matches[2], h), normTeams(matches[3], h))
  }
  else {
    NA
  }
}

constructTeamHash <- function() {
  h <- hash()
  h["Afr. Rep."] <- "Central African Republic"
  h["Antigua/Barbuda"] <- "Antigua and Barbuda"
  h["Br. Virgin Islands"] <- "British Virgin Islands"
  h["Bosnia and Herzegovina"] <- "Bosnia-Herzegovina"
  h["Bosnia-H."] <- "Bosnia-Herzegovina"
  h["Bosnia-Hercegovina"] <- "Bosnia-Herzegovina"
  h["Central Afr. Rep."] <- "Central African Republic"
  h["Congo-Brazzaville"] <- "Republic of the Congo"
  h["Congo-Kinshasa"] <- "Democratic Republic of the Congo"
  h["Czech Rep."] <- "Czech Republic"
  h["DR Congo"] <- "Democratic Republic of the Congo"
  h["Dutch Antilles"] <- "Netherlands Antilles"
  h["Faroe Isl."] <- "Faroe Islands"
  h["FR Yugoslavia"] <- "Yugoslavia"
  h["Gambia"] <- "The Gambia"
  h["Guinea Bissau"] <- "Guinea-Bissau"
  h["Ireland"] <- "Republic of Ireland"
  h["Hongkong"] <- "Hong Kong"
  h["Liechtenst."] <- "Liechtenstein"
  h["Macao"] <- "Macau"
  h["N. Ireland"] <- "Northern Ireland"
  h["Sierre Leone"] <- "Sierra Leone"
  h["Saint Vincent"] <- "Saint Vincent and the Grenadines"
  h["So Tom e Prncipe"] <- "Sao Tome and Principe"
  h["So Tom/Prncipe"] <- "Sao Tome and Principe"
  h["St Kitts and Nevis"] <- "Saint Kitts and Nevis"
  h["St. Kitts/Nevis"] <- "Saint Kitts and Nevis"
  h["Surinam"] <- "Suriname'"
  h["Trinidad/Tobago"] <- "Trinidad and Tobago"
  h["Turks and Caicos"] <- "Turks and Caicos Islands"
  h["Turks and Caicos Island"] <- "Turks and Caicos Islands"
  h["UAE"] <- "United Arab Emirates"
  h["US Virgin Islands"] <- "United States Virgin Islands"
  h["U.S. Virgin Islands"] <- "United States Virgin Islands"
  h["USA"] <- "United States"
  h["Virgin Islands"] <- "British Virgin Islands"
  h
}

normTeams <- function(team, h) {
  team <- iconv(team, to = "ASCII", sub = "")

  if (has.key(team, h)) {
    h[[team]]
  }
  else {
    team
  }
}
