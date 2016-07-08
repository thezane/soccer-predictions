function [a d] = computeStr(A, a, d, alphas, nu, c)
  aPrev = a;
  dPrev = d;
  alphasMat = [alphas; alphas];
  
  while (true)
    [aNext dNext] = computeAD(A, a, d, c);
    aDel = aNext - a;
    dDel = dNext - d;
    
    if (norm(aDel) < nu && norm(dDel) < nu)
      strNext = [aNext dNext];
      str = [aPrev dPrev];
      str = alphasMat .* strNext + (1 - alphasMat) .* str;   
      a = str(:, 1);
      d = str(:, 2);
      return;
    end   
    
    a = aNext;
    d = dNext;
  end
end

function [a d] = computeAD(A, a, d, c)
  tol = sqrt(eps);
  A = A + c * fliplr(eye(2));
  [aRelA dRelA] = scaleCol(A, a, tol);
  [dRelD aRelD] = scaleCol(A', d, tol);
  aNext = (aRelA + aRelD) / 2;
  dNext = (dRelA + dRelD) / 2;
  a = aNext;
  d = dNext;
end

function [x y] = scaleCol(A, x, tol)
  y = A * (1 ./ x);
  
  while (true)
    xNext = A' * (1 ./ y);
    yNext = A * (1 ./ x);
    xDel = xNext - x;
    yDel = yNext - y;
    x = xNext;
    y = yNext;
    
    if (norm(xDel) < tol && norm(yDel) < tol)
      return;
    end
  end
end
