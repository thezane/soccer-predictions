classdef RatingsOptions
  properties
    nu
    lambda
    k
    homeAdvantage
    contests
    contestWeights
    contestCosts
    daysStart
  end
  
  methods
    function rOptions = RatingsOptions(contests, qTCostRatio, ...
        daysStart)
      rOptions.contests = contests;
      rOptions.contestCosts = containers.Map(rOptions.contests, ...
          {qTCostRatio, qTCostRatio, 1, 1});
      rOptions.daysStart = daysStart;
    end
    
    function rOptions = update(rOptions, nu, lambda, k, ...
        homeAdvantage, qWeight, tWeight)
      rOptions.nu = nu;
      rOptions.lambda = lambda;
      rOptions.k = k;
      rOptions.homeAdvantage = homeAdvantage;
      rOptions.contestWeights = containers.Map(rOptions.contests, ...
          {qWeight, qWeight, tWeight, tWeight});
    end
  end
end
