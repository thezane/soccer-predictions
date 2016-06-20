function [a d] = updateStr(A, a, d, alpha)
  aPrev = a;
  dPrev = d;
  tol = 0.01;
  
  while (1)
    [aNext dNext] = computeStr(A, a, d);
    aDel = aNext - a;
    dDel = dNext - d;
    
    if (norm(aDel) < tol && norm(dDel) < tol)
      strNext = [aNext dNext];
      str = [aPrev dPrev];
      str = alpha .* strNext + (1 - alpha) .* str;
      a = str(1: 2, 1);
      d = str(1: 2, 2);
      [a d]
      return
    end
    
    a = aNext;
    d = dNext;
  end
end

function [a d] = computeStr(A, a, d)
  tol = 1e-06;
  lambda = 0.5;
  A = A + lambda - lambda * eye(2);
  [aRelA dRelA] = computeStrRelA(A, a, tol);
  [aRelD dRelD] = computeStrRelD(A, d, tol);
  onesCol = ones(2, 1);
  aNext = (aRelA + aRelD) / 2;
  dNext = (dRelA + dRelD) / 2;
  a = aNext;
  d = dNext;
end

function [a d] = computeStrRelA(A, a, tol)
  d = A * (1 ./ a);
  
  while (1)
    aNext = A' * (1 ./ d);
    dNext = A * (1 ./ a);
    aDel = aNext - a;
    dDel = dNext - d;
    a = aNext;
    d = dNext;
    
    if (norm(aDel) < tol && norm(dDel) < tol)
      return;
    end
  end
end

function [a d] = computeStrRelD(A, d, tol)
  a = A' * (1 ./ d);
  
  while (1)
    dNext = A * (1 ./ a);
    aNext = A' * (1 ./ d);
    aDel = aNext - a;
    dDel = dNext - d;
    a = aNext;
    d = dNext;
    
    if (norm(aDel) < tol && norm(dDel) < tol)
      return;
    end
  end
end
