classdef RatingsOptions
  properties
    nu
    lambda
    k
    homeAdvantage
    contests
    contestWeights
    contestCosts
    winTiesRatio
  end
  
  methods
    function rOptions = RatingsOptions(qTCostRatio, winTiesRatio)
      rOptions.contests = {'EUC-Q', 'WOC-Q', 'EUC-G', 'EUC-K'};
      rOptions.contestCosts = containers.Map(rOptions.contests, ...
          {qTCostRatio, qTCostRatio, 1, 1});
      rOptions.winTiesRatio = winTiesRatio;
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
