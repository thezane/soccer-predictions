classdef RatingsOutput
  properties
    cost
    results
  end
  
  methods
    function rOutput = RatingsOutput(cost, results)
      rOutput.cost = cost;
      rOutput.results = results;
    end
  end
end 
