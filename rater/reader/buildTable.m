function T = buildTable(dataPath, fileName)
  filePath = strcat(dataPath, fileName);
  T = readtable(filePath);
end 
