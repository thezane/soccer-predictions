classdef RatingsOptions
  properties
    lambda
    k
    homeAdvantage
    qWeight
    tWeight
    maxGoals
    c
    nu
    qTCostRatio
    winTiesRatio
  end
  
  methods
    function rOptions = RatingsOptions(maxGoals, c, nu, ...
        qTCostRatio, winTiesRatio)
      rOptions.maxGoals = maxGoals;
      rOptions.c = c;
      rOptions.nu = nu;
      rOptions.qTCostRatio = qTCostRatio;
      rOptions.winTiesRatio = winTiesRatio;
    end
    
    function rOptions = update(rOptions, lambda, k, homeAdvantage, ...
        qWeight, tWeight)
      rOptions.lambda = lambda;
      rOptions.k = k;
      rOptions.homeAdvantage = homeAdvantage;
      rOptions.qWeight = qWeight;
      rOptions.tWeight = tWeight; 
    end
  end
end
