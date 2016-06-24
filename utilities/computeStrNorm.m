function strNorm = computeStrNorm(str)
  strNorm = str(1, :) ./ str(2, :);
end
