function [rOutput match] = updateCost(rOutput, rOptions, match)
  str = match.teamStr;
  strExpected = match.teamStrNext;
  contestCost = computeContestCost(match, rOptions);
  strNorm = computeStrNorm(str);
  strExpectedNorm = computeStrNorm(strExpected);
  strDifference = contestCost * computeStrDifference(strNorm);
  strDel = contestCost * norm(strNorm - strExpectedNorm);
  tieInflation = rOptions.winTiesRatio;
  [rOutput match] = evaluatePrediction(rOutput, match, ...
      strNorm, strDifference, strDel, tieInflation);
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
    strNorm, strDifference, strDel, tieInflation)
  goals = match.goals;
  
  if (goals(1) == goals(2))
    match.isCorrect = -1;
    rOutput.strCost = rOutput.strCost + tieInflation * strDifference;
    rOutput.strDelCost = rOutput.strDelCost + tieInflation * strDel;
    return;
  end
  
  isQualifier = match.isQualifier();
  isCorrect = (goals(1) < goals(2) && strNorm(1) < strNorm(2)) || ...
      (goals(1) > goals(2) && strNorm(1) > strNorm(2));
  match.isCorrect = isCorrect;
  
  if (isCorrect)
    rOutput.strCost = rOutput.strCost - strDifference;
    rOutput.qResults(1) = rOutput.qResults(1) + isQualifier;
    rOutput.tResults(1) = rOutput.tResults(1) + ~isQualifier;
  else
    rOutput.strCost = rOutput.strCost + strDifference;
    rOutput.qResults(2) = rOutput.qResults(2) + isQualifier;
    rOutput.tResults(2) = rOutput.tResults(2) + ~isQualifier;
  end
  
  rOutput.strDelCost = rOutput.strDelCost + strDel;
end
