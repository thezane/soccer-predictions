function [rOutput match] = updateCost(rOutput, rOptions, match, A)
  str = match.teamStr;
  strPost = match.teamStrPost;
  strNext = match.teamStrNext;
  alphas = [0.5 0.5]';
  strExpected = computeStrNext(str, strPost, alphas);
  strNorm = computeStrNorm(str);
  strNextNorm = computeStrNorm(strNext);
  strExpectedNorm = computeStrNorm(strExpected);
  [rOutput match] = evaluatePrediction(rOptions, rOutput, match, ...
      strNorm, strNextNorm, strExpectedNorm);
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
  alphas = [0.5 0.5]';
  [a d] = computeStr(A, a, d, alphas, rOptions.c, rOptions.tolRel);
  strExpected = [a d];
end

function [rOutput match] = evaluatePrediction(rOptions, rOutput, ...
    match, strNorm, strNextNorm, strExpectedNorm)
  contestCost = computeContestCost(match, rOptions);
  strDelCost = meanSquaredError(strNorm(1), strNextNorm(1)) + ...
      meanSquaredError(strNorm(2), strNextNorm(2));
  strCost = meanSquaredError(strNorm(1), strExpectedNorm(1)) + ...
      meanSquaredError(strNorm(2), strExpectedNorm(2));
  rOutput.strDelCost = rOutput.strDelCost + contestCost * strDelCost;
  rOutput.strCost = rOutput.strCost + contestCost * strCost;
  goals = match.goals;
  
  if (goals(1) == goals(2))
    return;
  end
  
  isCorrect = (goals(1) < goals(2) && strNorm(1) < strNorm(2)) || ...
      (goals(1) > goals(2) && strNorm(1) > strNorm(2));
  match.isCorrect = isCorrect;
  isQualifier = match.isQualifier();
  
  if (isCorrect)
    rOutput.qResults(1) = rOutput.qResults(1) + isQualifier;
    rOutput.tResults(1) = rOutput.tResults(1) + ~isQualifier;
  else
    rOutput.qResults(2) = rOutput.qResults(2) + isQualifier;
    rOutput.tResults(2) = rOutput.tResults(2) + ~isQualifier;
  end
end
