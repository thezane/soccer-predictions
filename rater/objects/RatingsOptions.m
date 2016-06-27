classdef RatingsOptions
  properties
    c
    lambda
    k
    homeAdvantage
    qWeight
    tWeight
    maxGoals
    nu
    qTCostRatio
    winTiesRatio
  end
  
  methods
    function rOptions = RatingsOptions(maxGoals, nu, qTCostRatio, ...
        winTiesRatio)
      rOptions.maxGoals = maxGoals;
      rOptions.nu = nu;
      rOptions.qTCostRatio = qTCostRatio;
      rOptions.winTiesRatio = winTiesRatio;
    end
    
    function rOptions = update(rOptions, c, lambda, k, ...
        homeAdvantage, qWeight, tWeight)
      rOptions.c = c;
      rOptions.lambda = lambda;
      rOptions.k = k;
      rOptions.homeAdvantage = homeAdvantage;
      rOptions.qWeight = qWeight;
      rOptions.tWeight = tWeight; 
    end
  end
end
