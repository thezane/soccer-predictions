function mse = computeMSE(x1, x2)
  costMat = (x2 - x1) .^ 2;
  mse = sum(costMat(:));
end
