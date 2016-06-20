function [a d] = updateStr(A, a, d, alpha)
  aPrev = a;
  dPrev = d;
  [aNext dNext] = computeStr(A, a, d);
  strNext = [aNext dNext];
  str = [aPrev dPrev];
  str = alpha .* strNext + (1 - alpha) .* str;
  a = str(:, 1);
  d = str(:, 2);
  [a d]'
end

function [a d] = computeStr(A, a, d)
  tol = 1e-06;
  lambda = 0.01;
  A = A + lambda * fliplr(eye(2));
  pause
  [aRelA dRelA] = computeStrRelA(A, a, tol);
  [aRelD dRelD] = computeStrRelD(A, d, tol);
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
