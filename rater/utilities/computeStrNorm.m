function strNorm = computeStrNorm(str)
  strNorm = log(str(1, :) ./ str(2, :));
end
