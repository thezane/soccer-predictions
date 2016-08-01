function rOutput = updateCost(rOutput, rOptions, match, A)
  str = match.teamStr;
  rOutput = rOutput.updateStrAll(str);
  
  if (match.isQualifier)
    return;
  end
  
  strPost = match.teamStrPost;
  alphas = [0.5 0.5]';
  strExpected = computeStrNext(str, strPost, alphas);
  rOutput = updateRatingsCost(rOptions, rOutput, match, ...
      str, strExpected);
end

function rOutput = updateRatingsCost(rOptions, rOutput, ...
    match, str, strExpected)
  strNorm = computeStrNorm(str);
  strExpectedNorm = computeStrNorm(strExpected);
  strCost = computeMSE(strNorm, strExpectedNorm);
  rOutput.strCost = rOutput.strCost + strCost;
end
