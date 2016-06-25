computeMeanGoals <- function (matches) {
  homeGoals <- aggregate(matches[, "HomeGoals"],
      list(matches$Contest), mean)
  awayGoals <- aggregate(matches[, "AwayGoals"],
      list(matches$Contest), mean)
  h <- hash()
  h["group"] <- mean(c(homeGoals[1, 2], awayGoals[1, 2]))
  h["knockout"] <- mean(c(homeGoals[2, 2], awayGoals[2, 2]))
  h["qHome"] <- mean(c(homeGoals[3, 2], homeGoals[4, 2]))
  h["qAway"] <- mean(c(awayGoals[3, 2], awayGoals[4, 2]))
  h
}
