function displayResults(vTree)
  years = cell2mat(keys(vTree));
  n = length(years);
  i = 1;
  
  while (i <= n)
    year = years(i);
    results = vTree(year);
    display(sprintf('%d %d %d %d %d', year, results));
    i = i + 1;
  end
end
