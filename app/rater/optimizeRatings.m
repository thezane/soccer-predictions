function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi)
  rOptions = RatingsOptions({'EUC-Q', 'WOC-Q', 'EUC-G', 'EUC-K'}, ...
      0.05, datenum('2004-06-12', 'yyyy-mm-dd'));
  rOutput = RatingsOutput(0, [0 0]);
  nu = 0.32;
  lambda = 0.83;
  k = 0.87;
  homeAdvantage = 0.96;
  qWeight = 0.02;
  tWeight = 0.18;
  x0 = [nu lambda k homeAdvantage qWeight tWeight];
  diary on
  f = @(x) modelRatings(x, tTree, mTree, mi, rOptions, rOutput);
  x = fminsearch(f, x0)
  diary off
  [y tTree mTree mi rOptions rOutput] = modelRatings(x0, ...
    tTree, mTree, mi, rOptions, rOutput);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(x, ...
    tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3), x(4), x(5), x(6));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  y = rOutput.cost + rOutput.results(2) ^ 2;
  display('Model ratings');
  display(rOptions)
  display(cell2mat(values(rOptions.contestWeights)))
  display(rOutput)
  display(y)
end
