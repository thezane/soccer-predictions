newModel <- function(fTree) {
  model <- list(
    strBetas=c(0.4, 0.8),
    corrBeta=-5
  )
  class(model) <- "Model"
  model
}

updateModel <- function(model, strBeta) {
  model$strBetas <- c(strBeta, 2 * strBeta)
  model
}
