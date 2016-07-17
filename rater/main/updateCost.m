function rOutput = updateCost(rOutput, rOptions, match, A)
  str = match.teamStr;
  strPost = match.teamStrPost;
  strNext = match.teamStrNext;
  alphas = [0.5 0.5]';
  strExpected = computeStrNext(str, strPost, alphas);
  strNorm = computeStrNorm(str);
  strNextNorm = computeStrNorm(strNext);
  strExpectedNorm = computeStrNorm(strExpected);
  rOutput = updateRatingsCost(rOptions, rOutput, match, ...
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

function rOutput = updateRatingsCost(rOptions, rOutput, ...
    match, strNorm, strNextNorm, strExpectedNorm)
  contestCost = computeContestCost(match, rOptions);
  strDelCost = meanSquaredError(strNorm(1), strNextNorm(1)) + ...
      meanSquaredError(strNorm(2), strNextNorm(2));
  strCost = meanSquaredError(strNorm(1), strExpectedNorm(1)) + ...
      meanSquaredError(strNorm(2), strExpectedNorm(2));
  rOutput.strDelCost = rOutput.strDelCost + contestCost * strDelCost;
  rOutput.strCost = rOutput.strCost + contestCost * strCost;
end