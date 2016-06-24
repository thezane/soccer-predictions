function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    winTiesRatio, numMatches, isOptimized)
  qTCostRatio = 0.1;
  rOptions = RatingsOptions(qTCostRatio, winTiesRatio);
  rOutput = RatingsOutput(0, zeros(1, 4), numMatches);
  nu = 0.7531;
  lambda = 0.1984;
  k = 1.1914;
  homeAdvantage = 0.6304;
  qWeight = 0.1077;
  tWeight = 0.2222;
  x = [nu lambda k homeAdvantage qWeight tWeight];
  
  if (isOptimized)
    options = optimset('Display', 'iter', 'TolFun', 0.1, 'TolX', 0.1);
    f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
    x = fminsearch(f, x, options)
  end
  
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
      tTree, mTree, mi, rOptions, rOutput);
  display(rOutput);
  display(cell2mat(values(rOptions.contestWeights)));
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3), x(4), x(5), x(6));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  [rOutput strMedian] = rOutput.updateStrMedian();
  p = 2 + norm([1 1] - strMedian);
  y = rOutput.cost + rOutput.results(2) ^ p;
end
