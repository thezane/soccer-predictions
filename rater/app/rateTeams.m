function [tTree mTree mi rOptions rOutput] = rateTeams(tTree, ...
    mTree, mi, rOptions, rOutput)
  tTree = resetRatings(tTree);
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    [A a d teamsXP match] = computeStrPrereqs(tTree, match, rOptions);
    [tTree match] = updateStr(tTree, match, A, a, d, rOptions, ...
        teamsXP);
    [rOutput match] = updateCost(rOutput, rOptions, match);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
  end
end

function [A a d teamsXP match] = computeStrPrereqs(tTree, match, ...
    rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  match.teamStr = [homeTeam.str; awayTeam.str];
  goals = match.goals; 
  goals = normGoals(goals, rOptions.maxGoals);
  k = rOptions.tK;

  if (match.isQualifier())
    goals(1) = rOptions.homeAdvantage * goals(1);
    k = rOptions.qK;
  end

  A = [0 goals(2); goals(1) 0];
  a = match.teamStr(:, 1);
  d = match.teamStr(:, 2);
  t = match.days - [homeTeam.updateDays awayTeam.updateDays];
  teamsXP = expDecay(t, k / 365, [homeTeam.xp awayTeam.xp]);
end

function [tTree match] = updateStr(tTree, match, A, a, d, ...
    rOptions, teamsXP)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  alphas = 1 ./ (1 + teamsXP);
  [a d] = computeStr(A, a, d, alphas, rOptions.c, rOptions.tolRel);
  match.teamStrNext = [a d];
  homeTeam.str = [a(1) d(1)];
  awayTeam.str = [a(2) d(2)];
  homeTeam.xp = teamsXP(1) + 1;
  awayTeam.xp = teamsXP(2) + 1;
  homeTeam.updateDate = match.date;
  awayTeam.updateDate = match.date;
  homeTeam.updateDays = match.days;
  awayTeam.updateDays = match.days;
  tTree(match.teamNames{1}) = homeTeam;
  tTree(match.teamNames{2}) = awayTeam;
end
