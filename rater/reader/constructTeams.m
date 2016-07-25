function [tTree fTree] = constructTeams(dataPath)
  tTree = buildType2AnyMap('char');
  fTree = buildType2AnyMap('char');
  T = buildTable(dataPath, 'teams.csv');
  n = height(T);
  i = 1;
  
  while (i <= n)
    [tTree fName] = addTeam(T, i, tTree);
    fTree = addFederation(fTree, fName);
    i = i + 1;
  end
end

function [tTree fName] = addTeam(T, i, tTree)
  teamName = char(T{i, 'Team'});
  fName = char(T{i, 'Federation'});
  tTree(teamName) = Team(T, teamName, fName);
end

function fTree = addFederation(fTree, fName)
  if (~isKey(fTree, fName))
    fTree(fName) = [1 1];
  end
end
