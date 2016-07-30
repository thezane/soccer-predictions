function [mTree mi] = normalizeMatchGoals(mTree, mi, hA)
  [qHA tHA] = hA.computeHA();
  mi = mi.reset();

  while (mi.hasNext())
    [mi match] = mi.next();
    match.goalsNorm = computeGoalsNorm(match, qHA, tHA);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
  end
end

function goals = computeGoalsNorm(match, qHA, tHA)
  goals = match.goals;
  
  if (match.isQualifier)
    goals(1) = goals(1) / qHA;
  elseif (match.existsHomeAdvantage)
    goals(1) = goals(1) / tHA;
  end
end
