function [tTree mTree mi] = optimizeRatings(tTree, fTree, ...
    mTree, mi, numMatches)
  tolRel = 1e-04;
  tolScale = 1e-04;
  rOptions = RatingsOptions(fTree, tolRel, tolScale);
  rOutput = RatingsOutput(tTree, mTree, mi, numMatches);
  constrainedF = @(x, penalty, numFs) modelRatings(x, penalty, ...
      rOptions, rOutput);
  x = minimize(constrainedF, rOptions.numFs);
  penalty = 0;
  [y rOptions rOutput] = modelRatings(x, penalty, rOptions, rOutput);
  tTree = rOutput.tTree;
  mTree = rOutput.mTree;
  mi = rOutput.mi;
  display(rOptions);
  display(rOutput);
end

function [y rOptions rOutput] = modelRatings(x, penalty, ...
    rOptions, rOutput)
  rOptions = rOptions.update(x(1), x(2), x(3: end));
  [rOptions rOutput] = rateTeams(rOptions, rOutput);
  strCost = rOutput.strCost;
  [rOutput strMedianCost] = rOutput.updateStrMedianCost();
  medianConstraint = min(0, 1 - 50 * strMedianCost);
  y = strCost + penalty * medianConstraint ^ 2;
end

function x = minimize(constrainedF, numFs)
  k = 1;
  c = 0.5;
  strFs = ones(1, numFs);
  x = [k c strFs]';
  options = optimset;
  options.TolFun = 0.01;
  options.TolX = 0.01;
  tolPenalty = 0.01;
  penalty = 1;
  penaltyGrowth = 10;
  format = strcat(repmat('%0.4f    ', 1, 1 + length(x)));

  while (true)
    f = @(x) constrainedF(x, penalty, numFs);
    [xNext fXNext] = fminsearch(f, x, options);
    display(sprintf(format, fXNext, xNext));
    xDel = xNext - x;
    x = xNext;
    
    if (norm(xDel) < tolPenalty)
      return;
    end
    
    penalty = penaltyGrowth * penalty;
  end
end
