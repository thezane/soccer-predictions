getModel <- function(class) {
  UseMethod("getModel")
}

getModelLBd <- function(class) {
  UseMethod("getModelLBd")
}

getModelUBd <- function(class) {
  UseMethod("getModelUBd")
}

update <- function(class, x) {
  UseMethod("update")
}
