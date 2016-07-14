function strNext = computeStrNext(str, strPost, alphas)
  alphasMat = [alphas alphas];
  strNext = alphasMat .* strPost + (1 - alphasMat) .* str;
end 
