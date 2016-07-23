function rOutput = updateCost(rOutput, rOptions, match, A)
  str = match.teamStr;
  rOutput = rOutput.updateStrAll(str);
  
  if (match.isQualifier)
    return;
  end
  
  strPost = match.teamStrPost;
  strNext = match.teamStrNext;
  alphas = [0.5 0.5]';
  strExpected = computeStrNext(str, strPost, alphas);
  strNorm = computeStrNorm(str);
  strNextNorm = computeStrNorm(strNext);
  strExpectedNorm = computeStrNorm(strExpected);
  rOutput = updateRatingsCost(rOptions, rOutput, match, ...
      strNorm, strNextNorm, strExpectedNorm);
end

function strExpected = computeStrExpected(A, a, d, rOptions)
  alphas = [0.5 0.5]';
  [a d] = computeStr(A, a, d, alphas, rOptions.c, rOptions.tolRel);
  strExpected = [a d];
end

function rOutput = updateRatingsCost(rOptions, rOutput, ...
    match, strNorm, strNextNorm, strExpectedNorm)  
  strDelCost = meanSquaredError(strNorm(1), strNextNorm(1)) + ...
      meanSquaredError(strNorm(2), strNextNorm(2));
  strCost = meanSquaredError(strNorm(1), strExpectedNorm(1)) + ...
      meanSquaredError(strNorm(2), strExpectedNorm(2));
  rOutput.strDelCost = rOutput.strDelCost + strDelCost;
  rOutput.strCost = rOutput.strCost + strCost;
end
