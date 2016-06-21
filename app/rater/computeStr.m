function [a d] = computeStr(A, a, d, alpha, nu, lambda)
  aPrev = a;
  dPrev = d;
  
  while (true)
    [aNext dNext] = computeAD(A, a, d, lambda);
    aDel = aNext - a;
    dDel = dNext - d;
    
    if (norm(aDel) < nu && norm(dDel) < nu)
      strNext = [aNext dNext];
      str = [aPrev dPrev];
      str = alpha .* strNext + (1 - alpha) .* str;   
      a = str(:, 1);
      d = str(:, 2);
      return;
    end   
    
    a = aNext;
    d = dNext;
  end
end

function [a d] = computeAD(A, a, d, lambda)
  tol = sqrt(eps);
  A = A + lambda * fliplr(eye(2));
  [aRelA dRelA] = computeStrRelA(A, a, tol);
  [aRelD dRelD] = computeStrRelD(A, d, tol);
  aNext = (aRelA + aRelD) / 2;
  dNext = (dRelA + dRelD) / 2;
  a = aNext;
  d = dNext;
end

function [a d] = computeStrRelA(A, a, tol)
  d = A * (1 ./ a);
  
  while (true)
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
  
  while (true)
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
