function [tTree mTree] = forecastRatings(currentDate)
  setup;
  [tTree fTree mTree T hA] = readData(currentDate, dataPath);
  mi = MatchIterator(mTree);
  [mTree mi] = normalizeMatchGoals(mTree, mi, hA);
  [tTree mTree mi] = optimizeRatings(tTree, fTree, mTree, mi, ...
      height(T));
  vTree = verifyModel(mi);
  displayResults(vTree);
  T = writeData(mi, T, dataPath);
end
