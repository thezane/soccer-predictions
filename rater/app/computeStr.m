function strPost = computeStr(A, str, c, tolRel)
  a = str(:, 1);
  d = str(:, 2);
  
  while (true)
    [aPost dPost] = computeAD(A, a, d, c);
    aDel = aPost - a;
    dDel = dPost - d;
    
    if (norm(aDel) < tolRel && norm(dDel) < tolRel)
      strPost = [aPost dPost];
      return;
    end   
    
    a = aPost;
    d = dPost;
  end
end

function [a d] = computeAD(A, a, d, c)
  tolScale = 1e-03;
  A = A + c * fliplr(eye(2));
  [aRelA dRelA] = scaleRating(A, a, tolScale);
  [dRelD aRelD] = scaleRating(A', d, tolScale);
  aPost = (aRelA + aRelD) / 2;
  dPost = (dRelA + dRelD) / 2;
  a = aPost;
  d = dPost;
end

function [x y] = scaleRating(A, x, tolScale)
  y = A * (1 ./ x);
  
  while (true)
    xPost = A' * (1 ./ y);
    yPost = A * (1 ./ x);
    xDel = xPost - x;
    yDel = yPost - y;
    x = xPost;
    y = yPost;
    
    if (norm(xDel) < tolScale && norm(yDel) < tolScale)
      return;
    end
  end
end
