function rOutput = updateCost(rOutput, rOptions, match, ...
    str, strExpected)
  contestCost = computeContestCost(match, rOptions);
  strNorm = computeStrNorm(str);
  strExpectedNorm = computeStrNorm(strExpected);
  strDifference = contestCost * computeStrDifference(strNorm);
  strDel = contestCost * norm(strNorm - strExpectedNorm);
  tieInflation = rOptions.winTiesRatio;
  rOutput = evaluatePrediction(rOutput, match, ...
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

function rOutput = evaluatePrediction(rOutput, match, ...
    strNorm, strDifference, strDel, tieInflation)
  goals = match.goals;
  
  if (goals(1) == goals(2))
    rOutput.strCost = rOutput.strCost + tieInflation * strDifference;
    rOutput.strDelCost = rOutput.strDelCost + tieInflation * strDel;
    return;
  end
  
  isCorrect = (goals(1) < goals(2) && strNorm(1) < strNorm(2)) || ...
      (goals(1) > goals(2) && strNorm(1) > strNorm(2));
  
  if (isCorrect && match.isQualifier())
    rOutput.strCost = rOutput.strCost - strDifference;
    rOutput.qResults(1) = rOutput.qResults(1) + 1;
  elseif (isCorrect)
    rOutput.strCost = rOutput.strCost - strDifference;
    rOutput.tResults(1) = rOutput.tResults(1) + 1;
  elseif (~isCorrect && match.isQualifier())
    rOutput.strCost = rOutput.strCost + strDifference;
    rOutput.qResults(2) = rOutput.qResults(2) + 1;
  elseif (~isCorrect)
    rOutput.strCost = rOutput.strCost + strDifference;
    rOutput.tResults(2) = rOutput.tResults(2) + 1;    
  end
  
  rOutput.strDelCost = rOutput.strDelCost + strDel;
end
