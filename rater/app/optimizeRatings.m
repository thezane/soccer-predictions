function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    winTiesRatio, numMatches, isOptimized)
  qTCostRatio = 0.1;
  rOptions = RatingsOptions(qTCostRatio, winTiesRatio);
  rOutput = RatingsOutput(0, zeros(1, 4), numMatches);
  nu = 0.8667;
  lambda = 0.1296;
  k = 0.8661;
  homeAdvantage = 0.6;
  qWeight = 0.1379;
  tWeight = 0.3151;
  x = [nu lambda k homeAdvantage qWeight tWeight]';
  
  if (isOptimized)
    f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
    x = findMinimizer(f, x);
  end
  
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
      tTree, mTree, mi, rOptions, rOutput);
  display(rOutput);
  display(cell2mat(values(rOptions.contestWeights)));
end

function x = findMinimizer(f, x)
  xLbd = [0.01 0.1 0.5 0.5 0.01 0.1]';
  xUbd = [1 0.4 1 1 0.25 0.5]';
  printLevel = 2;
  n = length(x);
  numLevels = 5 * n + 10;
  maxFunCalls = 100 * n;
  stop = 2 * n;
  x = mcs(f, [], xLbd, xUbd, printLevel, numLevels, maxFunCalls, stop);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3), x(4), x(5), x(6));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  [rOutput strMedian] = rOutput.updateStrMedian();
  medianCost = norm([1 1] - strMedian);
  y = 1e+03 * medianCost + rOutput.cost + rOutput.results(2) ^ 2;
  display(x)
  display(y)
end
