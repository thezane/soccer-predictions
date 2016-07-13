function strNorm = computeStrNorm(str)
  strNorm = log(str(1, :)) - log(str(2, :));
end
