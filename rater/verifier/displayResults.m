function displayResults(vTree)
  format = repmat('%d    ', 1, 5);
  years = cell2mat(keys(vTree));
  n = length(years);
  i = 1;
  
  while (i <= n)
    year = years(i);
    results = vTree(year);
    display(sprintf(format, year, results));
    i = i + 1;
  end
end
