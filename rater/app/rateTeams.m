function [tTree mTree mi rOptions rOutput] = rateTeams(tTree, ...
    mTree, mi, rOptions, rOutput)
  tTree = resetRatings(tTree);
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    [A match] = computeStrPrereqs(tTree, match, rOptions);
    [tTree match] = updateStr(tTree, match, A, rOptions);
    [rOutput match] = updateCost(rOutput, rOptions, match, A);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
  end
end

function [A match] = computeStrPrereqs(tTree, match, rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  match.teamStr = [homeTeam.str; awayTeam.str];
  goals = normGoals(match.goals, rOptions.maxGoals);

  if (match.isQualifier())
    k = rOptions.qK;
    goals(1) = rOptions.homeAdvantage * goals(1);
  else
    k = rOptions.tK;
  end

  A = [0 goals(2); goals(1) 0];
  t = match.days - [homeTeam.updateDays awayTeam.updateDays]';
  match.teamXP = expDecay(t, k / 365, [homeTeam.xp awayTeam.xp]');
end

function [tTree match strPost] = updateStr(tTree, match, A, rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  str = match.teamStr;
  strPost = computeStr(A, str, rOptions.c, rOptions.tolRel);
  match.teamStrPost = strPost;
  teamXP = match.teamXP;
  alphas = 1 ./ (1 + teamXP);
  strNext = computeStrNext(str, strPost, alphas);
  match.teamStrNext = strNext;
  homeTeam.str = strNext(1, :);
  awayTeam.str = strNext(2, :);
  homeTeam.xp = teamXP(1) + 1;
  awayTeam.xp = teamXP(2) + 1;
  homeTeam.updateDate = match.date;
  awayTeam.updateDate = match.date;
  homeTeam.updateDays = match.days;
  awayTeam.updateDays = match.days;
  tTree(match.teamNames{1}) = homeTeam;
  tTree(match.teamNames{2}) = awayTeam;
end
