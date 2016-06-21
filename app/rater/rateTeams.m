function rateTeams(tTree, mTree)
  mi = MatchIterator(mTree);

  while (mi.hasNext())
    [mi match] = mi.next();
  end
end 
