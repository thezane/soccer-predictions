function T = writeData(mi, T, dataPath)
  NUM_DECIMALS = 4;
  colNames = {'HomeAttack', 'HomeDefense', ...
      'AwayAttack', 'AwayDefense', ...
      'HomeAttackNext', 'HomeDefenseNext', ...
      'AwayAttackNext', 'AwayDefenseNext'};
  T = addCols(T, colNames);
  mi = mi.reset();
  date = '';
  
  while (mi.hasNext())
    [mi match] = mi.next();
    T = updateMatches(T, match, NUM_DECIMALS);
    date = match.date;
  end
  
  T = sortrows(T, 'Date', 'descend');
  date = strrep(date, '/', '-');
  outFile = strcat(dataPath, 'sodm-', date, '.csv');
  write(T, outFile);
end

function T = addCols(T, colNames)
  onesCol = ones(height(T), 1);
  n = length(colNames);
  i = 1;
  
  while (i <= n)
    T{:, colNames{i}} = onesCol;
    i = i + 1;
  end
end

function T = updateMatches(T, match, numDecimals)
  i = match.row;
  teamStr = roundDecimals(log(match.teamStr), numDecimals);
  teamStrNext = roundDecimals(log(match.teamStrNext), numDecimals);
  T{i, 'HomeTeam'} = quoteStr(T{i, 'HomeTeam'});
  T{i, 'AwayTeam'} = quoteStr(T{i, 'AwayTeam'});
  T{i, 'Date'} = {quoteStr(match.date)};
  T{i, 'Contest'} = quoteStr(T{i, 'Contest'});
  T{i, 'HomeAttack'} = teamStr(1, 1);
  T{i, 'HomeDefense'} = teamStr(1, 2);
  T{i, 'AwayAttack'} = teamStr(2, 1);
  T{i, 'AwayDefense'} = teamStr(2, 2);
  T{i, 'HomeAttackNext'} = teamStrNext(1, 1);
  T{i, 'HomeDefenseNext'} = teamStrNext(1, 2);
  T{i, 'AwayAttackNext'} = teamStrNext(2, 1);
  T{i, 'AwayDefenseNext'} = teamStrNext(2, 2);    
end

function quotedStr = quoteStr(str)
  quotedStr = strcat('"', str, '"');
end
