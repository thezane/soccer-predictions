source("hindcastGames.R")
OPTIONS_DIR <- "options"
options_files <- list.files(OPTIONS_DIR)

for (options_file in options_files) {
  rOptions <- source(paste(OPTIONS_DIR, "/", options_file, sep=""))
  rData <- hindcastGames(rOptions)
}
