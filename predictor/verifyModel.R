verifyModel <- function (model) {
  residuals <- c(model$residuals[, "x"], model$residuals[, "y"])
  summary(residuals)
}
