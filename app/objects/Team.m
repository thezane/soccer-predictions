classdef Team
  properties
    name
    str
    updateDate
    updateDays
    xp
  end
  
  methods
    function team = Team(T, teamName)
      team.name = teamName;
      team.str = [1 1];
      team.updateDays = 0;
      team.xp = 0;
    end
  end
end
 
