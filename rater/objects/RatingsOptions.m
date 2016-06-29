classdef RatingsOptions
  properties
    lambda
    k
    homeAdvantage
    qTWeightRatio
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
        qTWeightRatio)
      rOptions.lambda = lambda;
      rOptions.k = k;
      rOptions.homeAdvantage = homeAdvantage;
      rOptions.qTWeightRatio = qTWeightRatio; 
    end
  end
end
