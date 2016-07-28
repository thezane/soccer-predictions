function strPost = computeStr(A, str, c, tolRel, tolScale)
  a = str(:, 1);
  d = str(:, 2);
  A = A + c * fliplr(eye(length(A)));

  while (true)
    [aPost dPost] = computeAD(A, a, d, c, tolScale);
    aDel = aPost - a;
    dDel = dPost - d;
    a = aPost;
    d = dPost;
    
    if (norm(aDel) < tolRel && norm(dDel) < tolRel)
      strPost = [aPost dPost];
      return;
    end 
  end
end

function [a d] = computeAD(A, a, d, c, tolScale)
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
