function x = bfgs(x, f, gradientF, hessianF, tol)
  fX = f(x);
  g = gradientF(x, fX);
  B = inv(hessianF(x, g));
  n = length(x);
  format = char(strcat({'%d    '}, ...
      {repmat('%0.4f    ', 1, 2 * n + 1)}));
  i = 0;

  while (true)
    display(sprintf(format, i, fX, g, x));
    p = -B * g;
    xNext = x + p;
    
    if (norm(g) < tol)
      return;
    end
    
    s = p;
    fXNext = f(xNext);
    gNext = gradientF(xNext, fXNext);
    y = gNext - g;
    roe = 1 / (y' * s);
    BNext = B - roe * ((B * y) * s' + s * (y' * B));
    BNext = BNext + s * ((roe ^ 2)  * y' * B * y + roe) * s';
    x = xNext;
    fX = fXNext;
    g = gNext;
    B = BNext;
    i = i + 1;
  end
end 
