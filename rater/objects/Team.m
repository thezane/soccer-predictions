classdef Team
  properties
    name
    str
    updateDate
    updateDays
    xp
    fName
  end
  
  methods
    function team = Team(T, teamName, fName)
      team.name = teamName;
      team.updateDays = 0;
      team.xp = 0;
      team.fName = fName;
    end
  end
end
 
