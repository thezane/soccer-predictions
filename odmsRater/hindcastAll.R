hindcastAll <- function() {
  ENV <- globalenv()
  COMPUTERS_DIR <- "computers"
  OPTIONS_DIR <- "options"
  computers_files <- list.files(COMPUTERS_DIR)
  options_files <- list.files(OPTIONS_DIR)
  sourceFiles(list.files(COMPUTERS_DIR), COMPUTERS_DIR, ENV)
  sourceFiles(list.files(OPTIONS_DIR),OPTIONS_DIR, ENV)
  function_names <- ls(ENV)
  options_constructor_names <- function_names[sapply(
      function_names,
      function(function_name) {
      startsWith(function_name, "new.RatingsOptions")})]
  source("hindcastGames.R", local=ENV)

  for (options_constructor_name in options_constructor_names) {
    options_constructor <- get(options_constructor_name)
    rData <- hindcastGames(options_constructor())
  }
}

sourceFiles <- function(files, dir, env) {
  for (file in files) {
    file_full <- paste(dir, "/", file, sep="")
    source(file_full, local=env)
  }
}
