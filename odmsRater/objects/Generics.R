updateGoalsCost <- function(obj, x1, x2)
    UseMethod("updateGoalsCost", obj)

updateGoalsCost <- function(obj, x1, x2, x3)
    UseMethod("updateGoalsCost", obj)

updateStrMeanCosts <- function(obj, x)
    UseMethod("updateStrMeanCosts", obj)

computeGoalsCost <- function(obj)
    UseMethod("computeGoalsCosts", obj)

computeGoalsCost <- function(obj, x)
    UseMethod("computeGoalsCosts", obj)

computeStrMeanCost <- function(obj)
    UseMethod("computeStrMeanCost", obj)

computeStrMeanCost <- function(obj, x)
    UseMethod("computeStrMeanCost", obj)
