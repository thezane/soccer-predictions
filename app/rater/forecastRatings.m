function [tTree mTree] = forecastRatings()
  setup;
  [tTree mTree] = readData(dataPath);
  mi = MatchIterator(mTree);
  [tTree mTree mi] = optimizeRatings(tTree, mTree, mi);
end 
