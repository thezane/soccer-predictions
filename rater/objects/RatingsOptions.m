classdef RatingsOptions
  properties
    qK
    tK
    c
    tolRel
    qTCostRatio
  end
  
  methods
    function rOptions = RatingsOptions(qTCostRatio, tolRel)
      rOptions.qTCostRatio = qTCostRatio;
      rOptions.tolRel = tolRel;
    end
    
    function rOptions = update(rOptions, qK, tK, c)
      rOptions.qK = qK;
      rOptions.tK = tK;
      rOptions.c = c;
    end
  end
end
