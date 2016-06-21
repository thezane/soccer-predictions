function strNorm = computeRatingsNorm(str)
  a = str(:, 1);
  d = str(:, 2);
  aNorm = a - 1;
  dNorm = 1 ./ d - 1;
  strNorm = [aNorm dNorm];
end
