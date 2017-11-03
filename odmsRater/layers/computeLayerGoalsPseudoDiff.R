computeLayerGoalsPseudoDiff <- function(game, rOptions) {
  goalsOdm <- game$goalsOdm

  if (goalsOdm[1] <= goalsOdm[2]) {
    minGoalsI = 1
    maxGoalsI = 2
  }
  else {
    minGoalsI = 2
    maxGoalsI = 1
  }

  goalsDiff <- max(goalsOdm) - min(goalsOdm)
  goalsOdm[minGoalsI] <- 0
  goalsOdm[maxGoalsI] <- computeGeomSeriesPartialSum(goalsDiff, rOptions$r)
  game$goalsOdm <- goalsOdm
  game
}

computeGeomSeriesPartialSum <- function(n, r) {
  (1 - r ^ n) / (1 - r)
}
