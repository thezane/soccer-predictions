function [tTree mTree] = forecastRatings(isOptimized, isSaved, ...
    exemptedTeams)
  setup;
  [tTree mTree T winTiesRatio] = readData(dataPath, exemptedTeams);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      winTiesRatio, height(T), isOptimized);
  
  if (isSaved)
    T = writeData(mTree, mi, T, dataPath);
  end
end 
