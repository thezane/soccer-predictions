function [tTree mTree mi] = optimizeRatings(tTree, mTree, mi, ...
    numMatches)
  qTCostRatio = 0;
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
  qK = 0.3;
  tK = 3;
  c = 0.3;
  x = [qK tK c]';
  options = optimset;
  options.TolFun = 1e-03;
  options.TolX = 1e-03;
  tolPenalty = 1e-03;
  penalty = 1;
  penaltyGrowth = 10;
  format = strcat(repmat('%0.4f    ', 1, 1 + length(x)));

  while (true)
    f = @(x) constrainedF(x, penalty);
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
