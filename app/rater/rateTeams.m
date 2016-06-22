function [tTree mTree rOptions rOutput] = rateTeams(tTree, mTree, ...
    mi, rOptions, rOutput)
  tTree = resetRatings(tTree);
  
  while (mi.hasNext())
    [mi match] = mi.next();
    [tTree match] = updateTeams(tTree, match, rOptions);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
    
    if (match.days < rOptions.daysStart)
      continue;
    end
    
    strNorm = computeRatingsNorm(match.teamStr);
    rOutput.results = updateResults(rOutput.results, match, ...
        strNorm, rOptions.contestCosts(match.contest));
  end
end

function [tTree match] = updateTeams(tTree, match, rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  [homeTeam awayTeam match] = updateStr(homeTeam, awayTeam, match, ...
      rOptions);
  homeTeam.updateDate = match.date;
  awayTeam.updateDate = match.date;
  homeTeam.updateDays = match.days;
  awayTeam.updateDays = match.days;
  tTree(match.teamNames{1}) = homeTeam;
  tTree(match.teamNames{2}) = awayTeam;
end

function results = updateResults(results, match, strNorm, ...
    contestCost)
  winTeamI = 1;
  loseTeamI = 2;

  if (match.goals(1) == match.goals(2))
    return;
  elseif (match.goals(1) < match.goals(2))
    winTeamI = 2;
    loseTeamI = 1;
  end
  
  strAvg = mean(strNorm');
  isCorrect = strAvg(winTeamI) > strAvg(loseTeamI);
  results(1) = results(1) + contestCost * isCorrect;
  results(2) = results(2) + contestCost * ~isCorrect;
end

function [homeTeam awayTeam match] = updateStr(homeTeam, awayTeam, ...
    match, rOptions)
  match.teamStr = [homeTeam.str; awayTeam.str];
  A = [0 match.goals(2); match.goals(1) 0];
  a = match.teamStr(:, 1);
  d = match.teamStr(:, 2);
  t = match.days - [homeTeam.updateDays awayTeam.updateDays];
  teamsXP = expDecay(t, rOptions.k / 365, [homeTeam.xp awayTeam.xp]);
  contestWeight = rOptions.contestWeights(match.contest);
  alphas = contestWeight ./ (teamsXP + contestWeight);
  [a d] = computeStr(A, a, d, alphas, rOptions.nu, rOptions.lambda);
  match.teamStrNext = [a d];
  homeTeam.str = [a(1) d(1)];
  awayTeam.str = [a(2) d(2)];
  homeTeam.xp = teamsXP(1) + contestWeight;
  awayTeam.xp = teamsXP(2) + contestWeight;
end
