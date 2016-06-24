function tTree = resetRatings(tTree)
  teams = keys(tTree);
  n = length(teams);
  i = 1;
  
  while (i <= n)
    team = tTree(teams{i});
    team.str = [1 1];
    team.updateDate = [];
    team.xp = 0;
    tTree(teams{i}) = team;
    i = i + 1;
  end
end 
