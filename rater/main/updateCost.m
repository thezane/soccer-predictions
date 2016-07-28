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

function strExpected = computeStrExpected(A, a, d, rOptions)
  alphas = [0.5 0.5]';
  [a d] = computeStr(A, a, d, alphas, rOptions.c, rOptions.tolRel);
  strExpected = [a d];
end

function rOutput = updateRatingsCost(rOptions, rOutput, ...
    match, str, strExpected)  
  strCost = meanSquaredError(str, strExpected);
  rOutput.strCost = rOutput.strCost + strCost;
end
