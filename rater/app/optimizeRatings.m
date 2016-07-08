function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    homeAdvantage, qTRatio, winTiesRatio, numMatches)
  maxGoals = 2;
  nu = 1e-06;
  qTCostRatio = 1 / qTRatio;
  rOptions = RatingsOptions(maxGoals, homeAdvantage, nu, ...
      qTCostRatio, winTiesRatio);
  rOutput = RatingsOutput(numMatches);
  f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
  x = findMinimizer(f);
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
      tTree, mTree, mi, rOptions, rOutput);
  display(rOptions);
  display(rOutput);
end

function x = findMinimizer(f)
  qKBds = [0.01 2]';
  tKBds = [2 4]';
  cBds = [0.2 0.4]';
  bds = [qKBds tKBds cBds]';
  printLevel = 2;
  n = length(bds);
  numLevels = 5 * n + 10;
  maxFunCalls = 50 * n;
  stop = n;
  x = mcs(f, [], bds(:, 1), bds(:, 2), printLevel, numLevels, ...
      maxFunCalls, stop);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  strCost = rOutput.strCost;
  strDelCost = rOutput.strDelCost;
  [rOutput strMedianCost] = rOutput.updateStrMedianCost();
  y = strCost + 1e+06 * (...
      (min(0, strCost - 4 * strDelCost)) ^ 2 + ...
      (min(0, 1 - 20 * strMedianCost)) ^ 2);
end
