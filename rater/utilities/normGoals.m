function goals = normGoals(goals, c, maxGoals)
  criticalGoals = [min([goals(1) maxGoals]) min([goals(2) maxGoals])];
  extraGoals = [max([goals(1) - maxGoals 0]) ...
      max([goals(2) - maxGoals 0])];
  goals = criticalGoals + c * log(extraGoals + 1);
end
