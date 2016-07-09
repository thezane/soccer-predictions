function [rOutput match] = updateCost(rOutput, rOptions, match)
  str = match.teamStr;
  strNext = match.teamStrNext;
  contestCost = computeContestCost(match, rOptions);
  strNorm = computeStrNorm(str);
  strNextNorm = computeStrNorm(strNext);
  strDifference = contestCost * computeStrDifference(strNorm);
  strDel = contestCost * norm(strNextNorm - strNorm);
  [rOutput match] = evaluatePrediction(rOutput, match, ...
      strNorm, strDifference, strDel);
  rOutput = rOutput.updateStrAll(str);
end

function contestCost = computeContestCost(match, rOptions)
  if (match.isQualifier())
    contestCost = rOptions.qTCostRatio;
  else
    contestCost = 1;
  end
end 

function [rOutput match] = evaluatePrediction(rOutput, match, ...
    strNorm, strDifference, strDel)
  goals = match.goals;
  rOutput.strDelCost = rOutput.strDelCost + strDel;
  
  if (goals(1) == goals(2))
    rOutput.strCost = rOutput.strCost + strDifference;
    return;
  end
  
  isQualifier = match.isQualifier();
  isCorrect = (goals(1) < goals(2) && strNorm(1) < strNorm(2)) || ...
      (goals(1) > goals(2) && strNorm(1) > strNorm(2));
  match.isCorrect = isCorrect;
  
  if (isCorrect)
    rOutput.strIncome = rOutput.strIncome + strDifference;
    rOutput.qResults(1) = rOutput.qResults(1) + isQualifier;
    rOutput.tResults(1) = rOutput.tResults(1) + ~isQualifier;
  else
    rOutput.strCost = rOutput.strCost + strDifference;
    rOutput.qResults(2) = rOutput.qResults(2) + isQualifier;
    rOutput.tResults(2) = rOutput.tResults(2) + ~isQualifier;
  end
end
