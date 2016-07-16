function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    qTRatio, numMatches)
  qTCostRatio = 1 / qTRatio;
  tolRel = 1e-03;
  rOptions = RatingsOptions(qTCostRatio, tolRel);
  rOutput = RatingsOutput(numMatches);
  constrainedF = @(x, penalty) modelRatings(x, penalty, ...
      tTree, mTree, mi, rOptions, rOutput);
  x = minimize(constrainedF);
  penalty = 0;
  [y tTree mTree mi rOptions rOutput] = modelRatings(x, penalty, ...
      tTree, mTree, mi, rOptions, rOutput);
  display(rOptions);
  display(rOutput);
end

function [y tTree mTree mi rOptions rOutput] = modelRatings(...
    x, penalty, tTree, mTree, mi, rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3));
  [tTree mTree mi rOptions rOutput] = rateTeams(tTree, mTree, mi, ...
      rOptions, rOutput);
  strCost = rOutput.strCost;
  strDelCost = rOutput.strDelCost;
  [rOutput strMedianCost] = rOutput.updateStrMedianCost();
  delConstraint = min(0, strCost - 2.5 * strDelCost);
  medianConstraint = 10 * min(0, 1 - 10 * strMedianCost);
  constraints = [delConstraint medianConstraint]';
  y = strCost + penalty * constraints' * constraints;
end

function x = minimize(constrainedF)
  qK = 1;
  tK = 3;
  c = 0.3;
  x = [qK tK c]';
  bdsHalfLength = [5 5 0.5]';
  minLBd = 0.01;
  printLevel = 1;
  n = length(x);
  numLevels = 5 * n + 10;
  maxFunCalls = 10 * n ^ 2;
  stop = n;
  linit = 2;
  local = 50;
  gamma = 1e-03;
  tolPenalty = 1e-03;
  penalty = 0.01;
  penaltyGrowth = 10;

  while (true)
    bds = [max(minLBd, x - bdsHalfLength) x + bdsHalfLength];
    f = @(x) constrainedF(x, penalty);
    xNext = mcs(f, [], bds(:, 1), bds(:, 2), printLevel, numLevels, ...
        maxFunCalls, stop, linit, local, gamma);
    xDel = xNext - x;
    x = xNext;
    
    if (norm(xDel) < tolPenalty)
      return;
    end
    
    penalty = penaltyGrowth * penalty;
  end
end
