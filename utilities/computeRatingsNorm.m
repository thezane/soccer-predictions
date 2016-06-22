function strNorm = computeRatingsNorm(str)
  a = str(:, 1);
  d = str(:, 2);
  aNorm = a;
  dNorm = 1 ./ d;
  strNorm = [aNorm dNorm];
end
