function [tTree mTree T homeAdvantage] = readData(currentDate, ...
    dataPath)
  tTree = buildType2AnyMap('char');
  mTree = buildType2AnyMap('char');
  homeAwayGoals = [0 0];
  numWins = 0;
  dateInFormat = 'mm/dd/yy';
  dateOutFormat = 'yyyy/mm/dd';
  currentDays = datenum(currentDate, dateInFormat);
  T = buildTable(dataPath, 'matches.csv');
  n = height(T);
  i = 1;
  
  while (i <= n)
    days = datenum(char(T{i, 'Date'}), dateInFormat);
    
    if (days <= currentDays)
      [tTree homeTeam] = addTeam(T, i, tTree, true);
      [tTree awayTeam] = addTeam(T, i, tTree, false);
      [mTree match] = addMatch(T, i, mTree, homeTeam, awayTeam, ...
          days, dateOutFormat);
      isQualifier = match.isQualifier();
      homeAwayGoals = homeAwayGoals + isQualifier * match.goals;
      i = i + 1;
    else
      T(i, :) = [];
      n = n - 1;
    end
    
  end
  
  homeAdvantage = homeAwayGoals(1) / homeAwayGoals(2);
end

function [tTree team] = addTeam(T, i, tTree, isHome)
  colHeader = 'Away';
  
  if (isHome)
    colHeader = 'Home';
  end

  teamName = char(T{i, strcat(colHeader, 'Team')});

  if (~isKey(tTree, teamName))
    tTree(teamName) = Team(T, teamName);
  end
  
  team = tTree(teamName);
end

function [mTree match] = addMatch(T, i, mTree, homeTeam, awayTeam, ...
    days, dateOutFormat)
  match = Match(T, i, homeTeam, awayTeam, days, dateOutFormat);
  
  if (~isKey(mTree, match.date))
    mTree(match.date) = [];
  end
  
  match.i = length(mTree(match.date)) + 1;
  mTree(match.date) = [mTree(match.date) match];
end
