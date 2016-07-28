function [mTree T hA] = constructMatches(currentDate, dataPath, tTree)
  mTree = buildType2AnyMap('char');
  hA = HomeAdvantage;
  dateInFormat = 'mm/dd/yy';
  dateOutFormat = 'yyyy/mm/dd';
  currentDays = datenum(currentDate, dateInFormat);
  T = buildTable(dataPath, 'matches.csv');
  n = height(T);
  i = 1;
  
  while (i <= n)
    days = datenum(char(T{i, 'Date'}), dateInFormat);
    
    if (days <= currentDays)
      homeTeam = tTree(char(T{i, 'HomeTeam'}));
      awayTeam = tTree(char(T{i, 'AwayTeam'}));
      [mTree match] = addMatch(T, i, mTree, homeTeam, awayTeam, ...
          days, dateOutFormat);
      hA = hA.updateHA(match);
      i = i + 1;
    else
      T(i, :) = [];
      n = n - 1;
    end
    
  end
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
