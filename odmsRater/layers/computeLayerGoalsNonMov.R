computeLayerGoalsNonMov <- function(game) {
  goalsOdm <- game$goalsOdm
  goalsOdm <- as.numeric(c(
      goalsOdm[1] > goalsOdm[2],
      goalsOdm[1] < goalsOdm[2]))
  game$goalsOdm <- goalsOdm
  game
}
