function T = writeData(mTree, mi, T)
  NUM_DECIMALS = 4;
  onesCol = ones(height(T), 1);
  T.HomeAttack = onesCol;
  T.HomeDefense = onesCol;
  T.AwayAttack = onesCol;
  T.AwayDefense = onesCol;
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    T = updateMatches(T, match, NUM_DECIMALS);
  end
  
  outFile = 'ratedMatches.csv';
  write(T, outFile);
end

function T = updateMatches(T, match, numDecimals)
  i = match.row;
  T{i, 'HomeTeam'} = quoteStr(T{i, 'HomeTeam'});
  T{i, 'AwayTeam'} = quoteStr(T{i, 'AwayTeam'});
  T{i, 'Date'} = {quoteStr(match.date)};
  T{i, 'Contest'} = quoteStr(T{i, 'Contest'});
  T{i, 'HomeAttack'} = roundDecimals(match.teamStr(1, 1), numDecimals);
  T{i, 'HomeDefense'} = roundDecimals(match.teamStr(1, 2), ...
      numDecimals);
  T{i, 'AwayAttack'} = roundDecimals(match.teamStr(2, 1), numDecimals);
  T{i, 'AwayDefense'} = roundDecimals(match.teamStr(2, 2), ...
      numDecimals);  
end

function quotedStr = quoteStr(str)
  quotedStr = strcat('"', str, '"');
end
