computeLayerGoalsDiff <- function(game) {
  goalsOdm <- game$goalsOdm
  goalsOdm <- goalsOdm - min(goalsOdm)
  game$goalsOdm <- goalsOdm
  game
}

