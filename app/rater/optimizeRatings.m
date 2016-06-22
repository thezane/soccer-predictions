function [tTree mTree] = optimizeRatings(tTree, mTree)
  contests = {'EUC-Q', 'WOC-Q', 'EUC-G', 'EUC-K'};
  rOptions = makeRatingsOptions(contests, 0, ...
      datenum('2004-06-12', 'yyyy-mm-dd'));
  nu = 0.51;
  lambda = 0.42;
  k = 0.9;
  qWeight = 0.15;
  tWeight = 0.52;
  rOptions = updateRatingsOptions(rOptions, contests, nu, lambda, ...
      k, qWeight, tWeight);
  rOutput = makeRatingsOutput(Inf, [0 0]);
  mi = MatchIterator(mTree);
  [tTree mTree rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  rOutput
end 

function rOptions = makeRatingsOptions(contests, qTCostRatio, ...
    daysStart)
  rOptions = RatingsOptions;
  rOptions.contestCosts = containers.Map(contests, ...
      {qTCostRatio, qTCostRatio, 1, 1});
  rOptions.daysStart = daysStart;
  rOptions.cost = Inf;
  rOptions.results = [0 0];
end

function rOutput = makeRatingsOutput(cost, results)
  rOutput = RatingsOutput;
  rOutput.cost = cost;
  rOutput.results = results;
end

function rOptions = updateRatingsOptions(rOptions, contests, ...
    nu, lambda, k, qWeight, tWeight)
  rOptions.nu = nu;
  rOptions.lambda = lambda;
  rOptions.k = k;
  rOptions.contestWeights = containers.Map(contests, ...
      {qWeight, qWeight, tWeight, tWeight});
end
