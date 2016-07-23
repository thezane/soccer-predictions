classdef RatingsOptions
  properties
    qK
    tK
    c
    tolRel
  end
  
  methods
    function rOptions = RatingsOptions(tolRel)
      rOptions.tolRel = tolRel;
    end
    
    function rOptions = update(rOptions, qK, tK, c)
      rOptions.qK = qK;
      rOptions.tK = tK;
      rOptions.c = c;
    end
  end
end
