function vTree = verifyModel(mi)
  vTree = buildType2AnyMap('int32');
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    
    if (match.isCorrect == -1)
      continue;
    elseif (~isKey(vTree, match.year))
      vTree(match.year) = zeros(1, 4);
    end
    
    if (match.isQualifier())
      result = [match.isCorrect ~match.isCorrect 0 0];
    else
      result = [0 0 match.isCorrect ~match.isCorrect];
    end
    
    vTree(match.year) = vTree(match.year) + result;
  end
end
