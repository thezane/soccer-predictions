function [tTree mTree] = forecastRatings(currentDate, isSaved)
  setup;
  [tTree mTree T winTiesRatio] = readData(currentDate, dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      winTiesRatio, height(T));
  
  if (isSaved)
    T = writeData(mTree, mi, T, dataPath);
  end
end 
