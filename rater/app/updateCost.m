function rOutput = updateCost(rOutput, rOptions, match, ...
    str, strExpected)
  diagonalInfatedConst = rOptions.winTiesRatio;
  strNorm = computeStrNorm(str);
  strExpectedNorm = computeStrNorm(strExpected);
  contestCost = computeContestCost(rOptions, match.contest);
  strDel = contestCost * norm(strNorm - strExpectedNorm);
  winTeamI = 1;
  loseTeamI = 2;
  rOutput = rOutput.updateStrAll(str);

  if (match.goals(1) == match.goals(2))
    rOutput.strDel = rOutput.strDel + diagonalInfatedConst * strDel;
    return;
  elseif (match.goals(1) < match.goals(2))
    winTeamI = 2;
    loseTeamI = 1;
  end

  rOutput.strDel = rOutput.strDel + strDel;
  
  if (match.isQualifier)
    rOutput.qResults = evaluatePrediction(rOutput.qResults, ...
        winTeamI, loseTeamI, strNorm, strExpectedNorm);
  else
    rOutput.tResults = evaluatePrediction(rOutput.tResults, ...
        winTeamI, loseTeamI, strNorm, strExpectedNorm);
  end
end

function contestCost = computeContestCost(contest, rOptions)
  if (strcmp(contest, 'EUC-Q') || strcmp(contest, 'WOC-Q'))
    contestCost = rOptions.qTCostRatio;
  else
    contestCost = 1;
  end
end 

function results = evaluatePrediction(results, winTeamI, ...
    loseTeamI, strNorm, strExpectedNorm)
  isCorrect = strNorm(winTeamI) > strNorm(loseTeamI);
  isExpectedCorrect = strExpectedNorm(winTeamI) > ...
      strExpectedNorm(loseTeamI);
  results = results + [isCorrect ~isCorrect ...
      isExpectedCorrect ~isExpectedCorrect];
end
