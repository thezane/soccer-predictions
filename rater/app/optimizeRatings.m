function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    homeAdvantage, qTRatio, numMatches)
  maxGoals = 3;
  tolRel = 1e-03;
  qTCostRatio = 1 / qTRatio;
  rOptions = RatingsOptions(maxGoals, homeAdvantage, tolRel, ...
      qTCostRatio);
  rOutput = RatingsOutput(numMatches);
  f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
  x = findMinimizer(f);
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
      tTree, mTree, mi, rOptions, rOutput);
  display(rOptions);
  display(rOutput);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  strIncome = rOutput.strIncome;
  strCost = rOutput.strCost;
  strDelCost = rOutput.strDelCost;
  [rOutput strMedianCost] = rOutput.updateStrMedianCost();
  y = strCost - strIncome + 1e+06 * (...
      (min(0, strCost - 5 * strDelCost)) ^ 2 + ...
      (min(0, 1 - 10 * strMedianCost)) ^ 2);
end

function x = findMinimizer(f)
  qK = 0.2;
  tK = 4;
  c = 0.2;
  x = [qK tK c]';
  e = 1e-06;
  tol = 1e-03;
  gradientF = @(x, fx) approxGradient(x, fx, f, e);
  hessianF = @(x, g) approxHessian(x, g, f, e);
  x = trustRegion(x, f, gradientF, hessianF, tol)
end
