function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    winTiesRatio, numMatches)
  maxGoals = 2;
  c = 1;
  nu = 1e-06;
  qTCostRatio = 0.01;
  rOptions = RatingsOptions(maxGoals, c, nu, qTCostRatio, ...
      winTiesRatio);
  rOutput = RatingsOutput(numMatches);
  f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
  x = findMinimizer(f);
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
      tTree, mTree, mi, rOptions, rOutput);
end

function x = findMinimizer(f)
  lambdaBds = [0.1 0.3]';
  kBds = [0.6 1]';
  homeAdvantageBds = [0.7 0.9]';
  qTWeightRatioBds = [0.2 0.6]';
  bds = [lambdaBds kBds homeAdvantageBds qTWeightRatioBds]';
  printLevel = 2;
  n = length(bds);
  numLevels = 5 * n + 10;
  maxFunCalls = 20 * n;
  stop = n;
  x = mcs(f, [], bds(:, 1), bds(:, 2), printLevel, numLevels, ...
      maxFunCalls, stop);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3), x(4));
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
