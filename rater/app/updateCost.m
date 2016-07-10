function [rOutput match] = updateCost(rOutput, rOptions, match, ...
    A, a, d)
  contestCost = computeContestCost(match, rOptions);
  str = match.teamStr;
  strNext = match.teamStrNext;
  strExpected = computeStrExpected(A, a, d, rOptions);
  strNorm = computeStrNorm(str);
  strNextNorm = computeStrNorm(strNext);
  strExpectedNorm = computeStrNorm(strExpected);
  strDifference = contestCost * meanSquaredError(...
      strNorm(1), strNorm(2));
  strExpectedDifference = contestCost * meanSquaredError(...
      strExpectedNorm(1), strExpectedNorm(2));
  strDel = contestCost * norm(strNextNorm - strNorm);
  [rOutput match] = evaluatePrediction(rOutput, match, ...
      strNorm, strDifference, strExpectedDifference, strDel);
  rOutput = rOutput.updateStrAll(str);
end

function contestCost = computeContestCost(match, rOptions)
  if (match.isQualifier())
    contestCost = rOptions.qTCostRatio;
  else
    contestCost = 1;
  end
end

function strExpected = computeStrExpected(A, a, d, rOptions)
  alphas = [0.5 0.5];
  [a d] = computeStr(A, a, d, alphas, rOptions.c, rOptions.tolRel);
  strExpected = [a d];
end

function [rOutput match] = evaluatePrediction(rOutput, match, ...
    strNorm, strDifference, strExpectedDifference, strDel)
  rOutput.strDelCost = rOutput.strDelCost + strDel;
  rOutput.strCost = rOutput.strCost + ...
      meanSquareError(strDifference, strExpectedDifference);
  goals = match.goals;
  
  if (goals(1) == goals(2))
    return;
  end
  
  isCorrect = (goals(1) < goals(2) && strNorm(1) < strNorm(2)) || ...
      (goals(1) > goals(2) && strNorm(1) > strNorm(2));
  isQualifier = match.isQualifier();
  
  if (isCorrect)
    rOutput.qResults(1) = rOutput.qResults(1) + isQualifier;
    rOutput.tResults(1) = rOutput.tResults(1) + ~isQualifier;
  else
    rOutput.qResults(2) = rOutput.qResults(2) + isQualifier;
    rOutput.tResults(2) = rOutput.tResults(2) + ~isQualifier;
  end
end
