function vTree = verifyModel(mi)
  vTree = buildType2AnyMap('int32');
  mi = mi.reset();
  
  while (mi.hasNext())
    [mi match] = mi.next();
    isCorrect = evaluatePrediction(match);
    
    if (isCorrect == -1)
      continue;
    elseif (~isKey(vTree, match.year))
      vTree(match.year) = zeros(1, 4);
    end
    
    if (match.isQualifier())
      result = [isCorrect ~isCorrect 0 0];
    else
      result = [0 0 isCorrect ~isCorrect];
    end
    
    vTree(match.year) = vTree(match.year) + result;
  end
end


function isCorrect = evaluatePrediction(match)
  goals = match.goals;
  
  if (goals(1) == goals(2))
    isCorrect = -1;
    return;
  else
    strNorm = computeStrNorm(match.teamStr);
    isCorrect = (goals(1) < goals(2) && strNorm(1) < strNorm(2)) || ...
        (goals(1) > goals(2) && strNorm(1) > strNorm(2));
  end
end
