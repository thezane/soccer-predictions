classdef RatingsOptions
  properties
    qK
    tK
    lambda
    homeAdvantage
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
    
    function rOptions = update(rOptions, qK, tK, ...
        lambda, homeAdvantage)
      rOptions.qK = qK;
      rOptions.tK = tK;
      rOptions.lambda = lambda;
      rOptions.homeAdvantage = homeAdvantage;
    end
  end
end
