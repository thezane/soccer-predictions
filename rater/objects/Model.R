newModel <- function(fTree) {
  model <- list(
    strBetas=c(0.5, 0.5),
    corrBeta=-5
  )
  class(model) <- "Model"
  model
}

updateModel <- function(model, strBetas, corrBeta) {
  model$strBetas <- strBetas
  model$corrBeta <- corrBeta
  model
}
