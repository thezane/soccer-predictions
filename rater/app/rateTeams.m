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
    rOutput = updateCost(rOutput, rOptions, match, match.teamStr, ...
        match.teamStrNext);
  end
end

function [A a d teamsXP match] = computeStrPrereqs(tTree, match, ...
    rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  match.teamStr = [homeTeam.str; awayTeam.str];
  goals = normGoals(match.goals, rOptions.c, rOptions.maxGoals);
  A = [0 goals(2); goals(1) 0];
  
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
  contestWeight = computeContestWeight(match.contest, rOptions);
  alphas = contestWeight ./ (teamsXP + contestWeight);
  [a d] = computeStr(A, a, d, alphas, rOptions.nu, rOptions.lambda);
  match.teamStrNext = [a d];
  homeTeam.str = [a(1) d(1)];
  awayTeam.str = [a(2) d(2)];
  homeTeam.xp = teamsXP(1) + contestWeight;
  awayTeam.xp = teamsXP(2) + contestWeight;
  homeTeam.updateDate = match.date;
  awayTeam.updateDate = match.date;
  homeTeam.updateDays = match.days;
  awayTeam.updateDays = match.days;
  tTree(match.teamNames{1}) = homeTeam;
  tTree(match.teamNames{2}) = awayTeam;
end

function contestWeight = computeContestWeight(contest, rOptions)
  if (strcmp(contest, 'EUC-Q') || strcmp(contest, 'WOC-Q'))
    contestWeight = rOptions.qWeight;
  else
    contestWeight = rOptions.tWeight;
  end
end
