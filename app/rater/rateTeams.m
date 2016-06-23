function [tTree mTree mi rOptions rOutput] = rateTeams(tTree, ...
    mTree, mi, rOptions, rOutput)
  tTree = resetRatings(tTree);
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    [A a d teamsXP match] = computeStrPrereqs(tTree, match, rOptions);
    [tTree match] = updateStr(tTree, match, A, a, d, rOptions, ...
        teamsXP);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
    
    if (match.days < rOptions.daysStart)
      continue;
    end
    
    strNorm = computeStrNorm(match.teamStr);
    strNormExpected = computeStrNormExpected(A, a, d, rOptions.lambda);
    rOutput = updateCost(rOutput, match, strNormExpected, strNorm, ...
        rOptions.contestCosts(match.contest));
  end
end

function [A a d teamsXP match] = computeStrPrereqs(tTree, match, ...
    rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  match.teamStr = [homeTeam.str; awayTeam.str];
  A = [0 match.goals(2); match.goals(1) 0];
  
  if (match.isQualifier())
    A(2, 1) = rOptions.homeAdvantage * A(2, 1);
  end
  
  a = match.teamStr(:, 1);
  d = match.teamStr(:, 2);
  t = match.days - [homeTeam.updateDays awayTeam.updateDays];
  teamsXP = expDecay(t, rOptions.k / 365, [homeTeam.xp awayTeam.xp]);
end

function [tTree match] = updateStr(tTree, match, A, a, d, ...
    rOptions, teamsXP)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  contestWeight = rOptions.contestWeights(match.contest);
  alphas = contestWeight ./ (teamsXP + contestWeight);
  [a d] = computeStr(A, a, d, alphas, rOptions.nu, rOptions.lambda);
  match.teamStrNext = [a d];
  homeTeam.str = [a(1) d(1)];
  awayTeam.str = [a(2) d(2)];
  contestWeight = rOptions.contestWeights(match.contest);
  homeTeam.xp = teamsXP(1) + contestWeight;
  awayTeam.xp = teamsXP(2) + contestWeight;
  homeTeam.updateDate = match.date;
  awayTeam.updateDate = match.date;
  homeTeam.updateDays = match.days;
  awayTeam.updateDays = match.days;
  tTree(match.teamNames{1}) = homeTeam;
  tTree(match.teamNames{2}) = awayTeam;
end

function strNormExpected = computeStrNormExpected(A, a, d, lambda)
  alphas = [1 1];
  nu = 1e-06;
  [a d] = computeStr(A, a, d, alphas, nu, lambda);
  strNormExpected = computeStrNorm([a d]);
end

function rOutput = updateCost(rOutput, match, strNormExpected, ...
    strNorm, contestCost)
  diagonalInfatedConst = 10;
  expectedActualCost = contestCost * norm(strNormExpected - strNorm);
  winTeamI = 1;
  loseTeamI = 2;

  if (match.goals(1) == match.goals(2))
    rOutput.cost = rOutput.cost + ...
        diagonalInfatedConst * expectedActualCost;
    return;
  elseif (match.goals(1) < match.goals(2))
    winTeamI = 2;
    loseTeamI = 1;
  end
  
  strAvg = mean(strNorm');
  isCorrect = strAvg(winTeamI) > strAvg(loseTeamI);
  results = rOutput.results;
  results(1) = results(1) + contestCost * isCorrect;
  results(2) = results(2) + contestCost * ~isCorrect;
  rOutput.results = results;
  rOutput.cost = rOutput.cost + expectedActualCost;
end
