function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    winTiesRatio, numMatches, isOptimized)
  qTCostRatio = 0.01;
  rOptions = RatingsOptions(qTCostRatio, winTiesRatio);
  rOutput = RatingsOutput(0, zeros(1, 4), numMatches);
  nu = 0.8278;
  lambda = 0.3;
  k = 0.9343;
  homeAdvantage = 0.7162;
  qWeight = 0.221;
  tWeight = 0.7041;
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
  nuBds = [0.5 1.3]';
  lambdaBds = [0.1 0.5]';
  kBds = [0.6 1.4]';
  homeAdvantageBds = [0.7 0.9]';
  qWeightBds = [0.1 0.3]';
  tWeightBds = [0.3 0.7]';
  bds = [nuBds lambdaBds kBds homeAdvantageBds qWeightBds tWeightBds]';
  printLevel = 2;
  n = length(x);
  numLevels = 5 * n + 10;
  maxFunCalls = 100 * n;
  stop = n;
  x = mcs(f, [], bds(:, 1), bds(:, 2), printLevel, numLevels, ...
      maxFunCalls, stop);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3), x(4), x(5), x(6));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  [rOutput strMedian] = rOutput.updateStrMedian();
  medianCost = 1 + norm([1 1] - strMedian);
  y = 100 * medianCost + rOutput.cost + rOutput.results(2) ^ 2;
  display(x)
  display(y)
end
