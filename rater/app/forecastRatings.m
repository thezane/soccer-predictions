function [tTree mTree] = forecastRatings(currentDate)
  setup;
  [tTree mTree T homeAdvantage qTRatio winTiesRatio] = readData(...
      currentDate, dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      homeAdvantage, qTRatio, winTiesRatio, height(T));
  T = writeData(mTree, mi, T, dataPath);
end 
