classdef Match
  properties
    contest
    date
    days
    year
    goals
    goalsNorm
    teamNames
    teamStr
    teamStrNext
    teamStrPost
    teamXP
    existsHomeAdvantage
    isQualifier
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
      match.isQualifier = length(regexp(match.contest, '-Q')) > 0;
      match.row = i;
    end
  end
end
