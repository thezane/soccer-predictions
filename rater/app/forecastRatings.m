function [tTree mTree] = forecastRatings(currentDate)
  setup;
  [tTree mTree T winTiesRatio] = readData(currentDate, dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      winTiesRatio, height(T));
  T = writeData(mTree, mi, T, dataPath);
end 
