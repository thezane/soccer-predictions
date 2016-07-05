function strDifference = computeStrDifference(strNorm)
  atanScaling = 1e+06;
  strDifference = atan(atanScaling * (strNorm(1) - strNorm(2)) ^ 2);
end 
