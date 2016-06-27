classdef RatingsOutput
  properties
    strDel
    qResults
    tResults
    strAll
    strMedian
    n
    i
  end
  
  methods
    function rOutput = RatingsOutput(numMatches)
      rOutput.strDel = 0;
      zerosRow = zeros(1, 4);
      rOutput.qResults = zerosRow;
      rOutput.tResults = zerosRow;
      rOutput.n = 2 * numMatches;
      rOutput.strAll = zeros(numMatches, 2);
      rOutput.i = 1;
    end
    
    function rOutput = updateStrAll(rOutput, str)
      if (rOutput.i + 1 <= rOutput.n)
        rOutput.strAll(rOutput.i: rOutput.i + 1, :) = str;
        rOutput.i = rOutput.i + 2;
      end
    end
    
    function [rOutput strMedian] = updateStrMedian(rOutput)
      rOutput.strMedian = median(rOutput.strAll);
      strMedian = rOutput.strMedian;
    end
  end
end 
