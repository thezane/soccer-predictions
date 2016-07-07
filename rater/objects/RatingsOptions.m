classdef RatingsOptions
  properties
    qK
    tK
    c
    homeAdvantage
    maxGoals
    nu
    qTCostRatio
    winTiesRatio
  end
  
  methods
    function rOptions = RatingsOptions(maxGoals, homeAdvantage, ...
        nu, qTCostRatio, winTiesRatio)
      rOptions.maxGoals = maxGoals;
      rOptions.homeAdvantage = homeAdvantage;
      rOptions.nu = nu;
      rOptions.qTCostRatio = qTCostRatio;
      rOptions.winTiesRatio = winTiesRatio;
    end
    
    function rOptions = update(rOptions, qK, tK, c)
      rOptions.qK = qK;
      rOptions.tK = tK;
      rOptions.c = c;
    end
  end
end
