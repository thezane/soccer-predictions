classdef HomeAdvantage
  properties
    qHomeGoals
    qAwayGoals
    numQsHA
    tHomeGoals
    tAwayGoals
    numTsHA
    tNeutralGoals
    numTsNeutral
  end
  
  methods
    function hA = HomeAdvantage()
      hA.qHomeGoals = 0;
      hA.qAwayGoals = 0;
      hA.numQsHA = 0;
      hA.tHomeGoals = 0;
      hA.tAwayGoals = 0;
      hA.numTsHA = 0;
      hA.tNeutralGoals = 0;
      hA.numTsNeutral = 0;
    end
    
    function hA = updateHA(hA, match)
      goals = match.goals;
      
      if (match.isQualifier)
        hA.qHomeGoals = hA.qHomeGoals + goals(1);
        hA.qAwayGoals = hA.qAwayGoals + goals(2);
        hA.numQsHA = hA.numQsHA + 1;
      elseif (match.existsHomeAdvantage)
        hA.tHomeGoals = hA.tHomeGoals + goals(1);
        hA.tAwayGoals = hA.tAwayGoals + goals(2);
        hA.numTsHA = hA.numTsHA + 1;
      else
        hA.tNeutralGoals = hA.tNeutralGoals + sum(goals);
        hA.numTsNeutral = hA.numTsNeutral + 1;
      end
    end
    
    function [qHA tHA] = computeHA(hA)
      qHA = hA.qHomeGoals / hA.qAwayGoals;
      tAwayGoals = hA.tAwayGoals + hA.tNeutralGoals;
      numTsAway = hA.numTsHA + 2 * hA.numTsNeutral;
      tHA = (hA.tHomeGoals / hA.numTsHA) / (tAwayGoals / numTsAway);
    end
  end
end
