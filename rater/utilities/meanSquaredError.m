function cost = meanSquaredError(x1, x2)
  costMat = (x2 - x1) .^ 2;
  cost = sum(costMat(:));
end
