function [tTree mTree T winTiesRatio] = readData(dataPath, ...
    exemptedTeams)
  tTree = buildType2AnyMap('char');
  mTree = buildType2AnyMap('char');
  numWins = 0;
  dateInFormat = 'mm/dd/yy';
  dateOutFormat = 'yyyy/mm/dd';
  T = buildTable(dataPath, 'matches.csv');
  n = height(T);
  i = 1;
  
  while (i <= n)
    teamNames = {char(T{i, 'HomeTeam'}) char(T{i, 'AwayTeam'})};
    
    if (~sum(ismember(teamNames, exemptedTeams)))
      [tTree homeTeam] = addTeam(T, i, tTree, teamNames{1}, true);
      [tTree awayTeam] = addTeam(T, i, tTree, teamNames{2}, false);
      [mTree numWins] = addMatch(T, i, mTree, homeTeam, awayTeam, ...
          numWins, dateInFormat, dateOutFormat);
      i = i + 1;
    else
      T(i, :) = [];
      n = n - 1;
    end
    
  end
  
  winTiesRatio = numWins / (n - numWins);
end

function [tTree team] = addTeam(T, i, tTree, teamName, isHome)
  colHeader = 'Away';
  
  if (isHome)
    colHeader = 'Home';
  end
  
  if (~isKey(tTree, teamName))
    tTree(teamName) = Team(T, teamName);
  end
  
  team = tTree(teamName);
end

function [mTree numWins] = addMatch(T, i, mTree, homeTeam, ...
    awayTeam, numWins, dateInFormat, dateOutFormat)
  match = Match(T, i, homeTeam, awayTeam, dateInFormat, dateOutFormat);
  
  if (~isKey(mTree, match.date))
    mTree(match.date) = [];
  end
  
  match.i = length(mTree(match.date)) + 1;
  mTree(match.date) = [mTree(match.date) match];
  numWins = numWins + (match.goals(1) ~= match.goals(2));
end
