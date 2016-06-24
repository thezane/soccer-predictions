function [tTree mTree] = forecastRatings(isOptimized, isUpdated)
  setup;
  [tTree mTree T winTiesRatio] = readData(dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      winTiesRatio, height(T), isOptimized);
  
  if (isUpdated)
    T = writeData(mTree, mi, T);
  end
end 
