newModel <- function(fTree) {
  model <- list(
    strBetas=c(0.5, 0.5),
    corrBeta=-5
  )
  class(model) <- "Model"
  model
}

updateModel <- function(model, strBeta, corrBeta) {
  model$strBetas <- c(strBeta, 2 * strBeta)
  model$corrBeta <- corrBeta
  model
}
