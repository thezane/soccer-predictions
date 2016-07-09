function [tTree mTree] = forecastRatings(currentDate)
  setup;
  [tTree mTree T homeAdvantage qTRatio] = readData(currentDate, ...
      dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
      homeAdvantage, qTRatio, height(T));
  displayResults(verifyModel(mi));
  T = writeData(mi, T, dataPath);
end 
