function [rOptions rOutput] = rateTeams(rOptions, rOutput)
  tTree = rOutput.tTree;
  mTree = rOutput.mTree;
  mi = rOutput.mi;
  mi = rOutput.mi;
  tTree = resetRatings(tTree);
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    [A match] = computeStrPrereqs(tTree, match, rOptions);
    [tTree match] = updateStr(tTree, match, A, rOptions);
    rOutput = updateCost(rOutput, rOptions, match, A);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
  end
  
  rOutput.tTree = tTree;
  rOutput.mTree = mTree;
  rOutput.mi = mi;
end

function [A match] = computeStrPrereqs(tTree, match, rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  fTree = rOptions.fTree;
  
  if (length(homeTeam.str) == 0)
    homeTeam.str = fTree(homeTeam.fName);
  end
  
  if (length(awayTeam.str) == 0)
    awayTeam.str = fTree(awayTeam.fName);
  end
  
  match.teamStr = [homeTeam.str; awayTeam.str];
  k = rOptions.k;
  goals = match.goalsNorm;
  A = [0 goals(2); goals(1) 0];
  t = match.days - [homeTeam.updateDays awayTeam.updateDays]';
  match.teamXP = expDecay(t, k / 365, [homeTeam.xp awayTeam.xp]');
end

function [tTree match strPost] = updateStr(tTree, match, A, rOptions)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  str = match.teamStr;
  strPost = computeStr(A, str, rOptions.c, ...
      rOptions.tolRel, rOptions.tolScale);
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
