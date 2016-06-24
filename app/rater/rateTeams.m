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
    strExpected = computeStrExpected(A, a, d, rOptions.lambda);
    rOutput = updateCost(rOutput, rOptions, match, match.teamStr, ...
        strExpected);
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

function strExpected = computeStrExpected(A, a, d, lambda)
  alphas = [1 1];
  nu = 1e-06;
  [a d] = computeStr(A, a, d, alphas, nu, lambda);
  strExpected = [a d];
end

function rOutput = updateCost(rOutput, rOptions, match, ...
    str, strExpected)
  contestCost = rOptions.contestCosts(match.contest);
  diagonalInfatedConst = rOptions.winTiesRatio;
  strNorm = computeStrNorm(str);
  strExpectedNorm = computeStrNorm(strExpected);
  expectedActualCost = contestCost * norm(strNorm - strExpectedNorm);
  winTeamI = 1;
  loseTeamI = 2;
  rOutput = rOutput.updateStrAll(str);

  if (match.goals(1) == match.goals(2))
    rOutput.cost = rOutput.cost + ...
        diagonalInfatedConst * expectedActualCost;
    return;
  elseif (match.goals(1) < match.goals(2))
    winTeamI = 2;
    loseTeamI = 1;
  end

  rOutput.cost = rOutput.cost + expectedActualCost;
  rOutput.results(1: 2) = evaluatePrediction(rOutput.results(1: 2), ...
      winTeamI, loseTeamI, contestCost, strNorm);
  rOutput.results(3: 4) = evaluatePrediction(rOutput.results(3: 4), ...
      winTeamI, loseTeamI, ~match.isQualifier(), strNorm);
end

function results = evaluatePrediction(results, ...
    winTeamI, loseTeamI, contestCost, strNorm)
  isCorrect = strNorm(winTeamI) > strNorm(loseTeamI);
  results(1) = results(1) + contestCost * isCorrect;
  results(2) = results(2) + contestCost * ~isCorrect;
end
