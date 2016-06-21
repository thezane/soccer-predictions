function evalRatings()
  d = datenum('2008-01-01', 'yyyy-mm-dd')
  mi = mi.reset();
  r = [0 0];
  
  while (mi.hasNext())
    [mi match] = mi.next();
    
    if ((strcmp(match.contest, 'EUC-G') || strcmp(match.contest, 'EUC-K')) && match.days > d)
      match;
      strNorm = computeRatingsNorm(match.teamStr);
      
      if (match.goals(1) > match.goals(2))
        strAvg = mean(strNorm');
        isCorrect = strAvg(1) > strAvg(2);
        r(1) = r(1) + isCorrect;
        r(2) = r(2) + ~isCorrect;
      elseif (match.goals(2) > match.goals(1))
        strAvg = mean(strNorm');
        isCorrect = strAvg(2) > strAvg(1);
        r(1) = r(1) + isCorrect;
        r(2) = r(2) + ~isCorrect;       
      end
    end
  end
  
  r
end 
