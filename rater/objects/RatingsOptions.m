classdef RatingsOptions
  properties
    qK
    tK
    c
    homeAdvantage
    maxGoals
    tolRel
    qTCostRatio
  end
  
  methods
    function rOptions = RatingsOptions(maxGoals, homeAdvantage, ...
        tolRel, qTCostRatio, winTiesRatio)
      rOptions.maxGoals = maxGoals;
      rOptions.homeAdvantage = homeAdvantage;
      rOptions.tolRel = tolRel;
      rOptions.qTCostRatio = qTCostRatio;
    end
    
    function rOptions = update(rOptions, qK, tK, c)
      rOptions.qK = qK;
      rOptions.tK = tK;
      rOptions.c = c;
    end
  end
end
