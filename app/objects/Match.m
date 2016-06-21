classdef Match
  properties
    contest
    date
    days
    teams
    goals
    teamStrOld
    teamStrNew
    homeAdvantage
  end
  
  methods
    function n = getSize(m)
      n = m.days;
    end
  end
end
