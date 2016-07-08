function [tTree mTree mi] = forecastRatings(currentDate)
  setup;
  [tTree mTree T homeAdvantage qTRatio winTiesRatio] = readData(...
      currentDate, dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      homeAdvantage, qTRatio, winTiesRatio, height(T));
  displayResults(verifyModel(mi));
  T = writeData(mi, T, dataPath);
end 
