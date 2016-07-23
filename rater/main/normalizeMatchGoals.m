function [mTree mi] = normalizeMatchGoals(mTree, mi, homeAdvantage)
  mi = mi.reset();
  c = 3 / log(10);

  while (mi.hasNext())
    [mi match] = mi.next();
    match.goalsNorm = computeGoalsNorm(match.goals, c, ...
        homeAdvantage, match.isQualifier);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
  end
end

function goals = computeGoalsNorm(goals, c, ...
    homeAdvantage, isQualifier)
  if (isQualifier)
    goals(1) = goals(1) / homeAdvantage;
  end
  
  goals = c * log(1 + goals);
end
