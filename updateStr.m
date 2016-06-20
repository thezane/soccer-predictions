function [a d dVs] = updateStr(A, a, d, alpha)
  tol = 1e-06;
  lambda = 0.01;
  k = 3;
  A = [A; zeros(k - 2, 2)];
  A = [A zeros(k, k - 2)];
  A = A + lambda;
  a = [a; ones(k - 2, 1)];
  d = [d; ones(k - 2, 1)];
  [aRelA dRelA] = updateStrRelA(A, a, tol);
  [aRelD dRelD] = updateStrRelD(A, d, tol);
  onesCol = ones(2, 1);
  aNext = (aRelA + aRelD) / 2;
  dNext = (dRelA + dRelD) / 2;
  strNext = [aNext dNext];
  str = [a d];
  str = alpha .* strNext + (1 - alpha) .* str;
  a = str(1: 2, 1);
  d = str(1: 2, 2);
  [a d]
end

function [a d] = updateStrRelA(A, a, tol)
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

function [a d] = updateStrRelD(A, d, tol)
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
