function [a d] = computeStr(A, a, d, alphas, c, tolRel)
  aPrev = a;
  dPrev = d;
  alphasMat = [alphas; alphas];
  
  while (true)
    [aNext dNext] = computeAD(A, a, d, c);
    aDel = aNext - a;
    dDel = dNext - d;
    
    if (norm(aDel) < tolRel && norm(dDel) < tolRel)
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
  tolScale = 1e-03;
  A = A + c * fliplr(eye(2));
  [aRelA dRelA] = scaleCol(A, a, tolScale);
  [dRelD aRelD] = scaleCol(A', d, tolScale);
  aNext = (aRelA + aRelD) / 2;
  dNext = (dRelA + dRelD) / 2;
  a = aNext;
  d = dNext;
end

function [x y] = scaleCol(A, x, tolScale)
  y = A * (1 ./ x);
  
  while (true)
    xNext = A' * (1 ./ y);
    yNext = A * (1 ./ x);
    xDel = xNext - x;
    yDel = yNext - y;
    x = xNext;
    y = yNext;
    
    if (norm(xDel) < tolScale && norm(yDel) < tolScale)
      return;
    end
  end
end
