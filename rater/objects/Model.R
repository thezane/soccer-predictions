newModel <- function(fTree) {
  model <- list(
    aBeta=0.5,
    dBeta=0.6,
    corrBeta=-5,
    p=0.05,
    theta=0.9
  )
  class(model) <- "Model"
  model
}

updateModel <- function(model, aBeta, dBeta, corrBeta, p) {
  model$aBeta <- aBeta
  model$dBeta <- dBeta
  model$corrBeta <- corrBeta
  model$p <- p
  model
}
