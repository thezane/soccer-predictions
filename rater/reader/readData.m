function [tTree fTree mTree T hA] = readData(currentDate, dataPath)
  [tTree fTree] = constructTeams(dataPath);
  [mTree T hA] = constructMatches(currentDate, dataPath, tTree);
end
