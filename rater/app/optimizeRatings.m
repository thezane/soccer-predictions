function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    winTiesRatio, numMatches, isOptimized)
  maxGoals = 3;
  nu = 1e-06;
  qTCostRatio = 0.01;
  rOptions = RatingsOptions(maxGoals, nu, qTCostRatio, winTiesRatio);
  rOutput = RatingsOutput(numMatches);
  c = 2;
  lambda = 0.3;
  k = 0.9217;
  homeAdvantage = 0.7;
  qWeight = 0.1;
  tWeight = 0.4233;
  x = [c lambda k homeAdvantage qWeight tWeight]';
  
  if (isOptimized)
    f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
    x = findMinimizer(f, x);
  end
  
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
      tTree, mTree, mi, rOptions, rOutput);
  display(rOptions);
  display(rOutput);
end

function x = findMinimizer(f, x)
  cBds = [0 2]';
  lambdaBds = [0.1 0.3]';
  kBds = [0.6 1.4]';
  homeAdvantageBds = [0.7 0.9]';
  qWeightBds = [0.1 0.3]';
  tWeightBds = [0.4 0.8]';
  bds = [cBds lambdaBds kBds homeAdvantageBds qWeightBds tWeightBds]';
  printLevel = 2;
  n = length(x);
  numLevels = 5 * n + 10;
  maxFunCalls = 20 * n;
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
  resultsCost = 50 * (rOptions.qTCostRatio * rOutput.qResults(2) + ...
      rOutput.tResults(2));
  strCost = rOutput.strDel;
  medianCost = 1e+03 * norm([1 1] - strMedian);
  y = resultsCost + strCost + medianCost;
  display(rOptions);
  display(rOutput);
end
