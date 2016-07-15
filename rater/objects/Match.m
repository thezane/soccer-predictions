classdef Match
  properties
    contest
    date
    days
    year
    goals
    teamNames
    teamStr
    teamStrNext
    teamStrPost
    teamXP
    existsHomeAdvantage
    i
    row
  end
  
  methods
    function match = Match(T, i, homeTeam, awayTeam, days, ...
        dateOutFormat)
      match.contest = char(T{i, 'Contest'});
      match.days = days;
      match.date = datestr(match.days, dateOutFormat);
      match.year = str2num(match.date(1: 4));
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
