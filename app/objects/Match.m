classdef Match
  properties
    contest
    date
    days
    teamNames
    goals
    teamStr
    teamStrNext
    homeAdvantage
    i
  end
  
  methods
    function n = getSize(m)
      n = m.days;
    end
  end
end
