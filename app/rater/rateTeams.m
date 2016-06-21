function rateTeams(tTree, mTree, rOptions)
  tTree = resetRatings(tTree);
  mi = MatchIterator(mTree);

  while (mi.hasNext())
    [mi match] = mi.next();
    [tTree match] = updateStr(tTree, match, rOptions.nu, ...
        rOptions.lambda);
    mList = mTree(match.date);
    mList(match.i) = match;
    mTree(match.date) = mList;
  end
end

function [tTree match] = updateStr(tTree, match, nu, lambda)
  homeTeam = tTree(match.teamNames{1});
  awayTeam = tTree(match.teamNames{2});
  match.teamStr = [homeTeam.str; awayTeam.str];
  A = [0 match.goals(2); match.goals(1) 0];
  a = match.teamStr(:, 1);
  d = match.teamStr(:, 2);
  alpha = 0.5;
  [a d] = computeStr(A, a, d, alpha, nu, lambda);
  match.teamStrNext = [a d];
  homeTeam.str = [a(1) d(1)];
  awayTeam.str = [a(2) d(2)];
  homeTeam.updateDate = match.date;
  awayTeam.updateDate = match.date;
  tTree(match.teamNames{1}) = homeTeam;
  tTree(match.teamNames{2}) = awayTeam;
end
