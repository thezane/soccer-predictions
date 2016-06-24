classdef Match
  properties
    contest
    date
    days
    teamNames
    goals
    teamStr
    teamStrNext
    existsHomeAdvantage
    i
    row
  end
  
  methods
    function match = Match(T, i, homeTeam, awayTeam, dateInFormat, ...
        dateOutFormat)
      match.contest = char(T{i, 'Contest'});
      match.days = datenum(char(T{i, 'Date'}), dateInFormat);
      match.date = datestr(match.days, dateOutFormat);
      match.teamNames = {homeTeam.name awayTeam.name};
      match.goals = [T{i, 'HomeGoals'} T{i, 'AwayGoals'}];
      match.existsHomeAdvantage = T{i, 'HomeAdvantage'};
      match.row = i;
    end

    function tf = isQualifier(match)
      tf = strcmp(match.contest, 'EUC-Q') || ...
          strcmp(match.contest, 'WOC-Q');
    end
  end
end
