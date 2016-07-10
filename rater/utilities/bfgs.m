function x = bfgs(x, f, gradientF, hessianF, tol)
  fX = f(x);
  g = gradientF(x);
  H = inv(hessianF(x, g));
  n = length(x);
  format = char(strcat({'%d    '}, ...
      {repmat('%0.4f    ', 1, 2 * (n + 1))}));
  i = 0;

  while (true)
    p = -H * g;
    [alpha fXNext] = lineSearch(x, p, g, f, fX);
    display(sprintf(format, i, g, alpha, fX, x));
    xNext = x + alpha * p;
    
    if (norm(g) < tol)
      return;
    end
    
    s = p;
    gNext = gradientF(xNext);
    y = gNext - g;
    roe = 1 / (y' * s);
    HNext = H - roe * ((H * y) * s' + s * (y' * H));
    HNext = HNext + s * ((roe ^ 2)  * y' * H * y + roe) * s';
    x = xNext;
    fX = fXNext;
    g = gNext;
    H = HNext;
    i = i + 1;
  end
end

function [alpha phiAlpha] = lineSearch(x, p, g, f, fx)
  phi = @(alpha) f(x + alpha * p);
  alphaNext = 1;
  c1 = 1e-04;
  phi0 = fx;
  phiFirst0 = g' * p;
  phiAlphaNext = phi(alphaNext);
  alpha = alphaNext;
  phiAlpha = phiAlphaNext;
  i = 1;
  
  while (~isSufficientDecrease(alphaNext, c1, phi0, phiFirst0, ...
      phiAlphaNext))
    if (i > 1 && ~isGoldilocks(alphaNext, alpha))
      alphaNext = alpha / 2;
      phiAlphaNext = phi(alphaNext);
    end

    if (mod(i, 2) == 1)
      alpha = alphaNext;
      phiAlpha = phiAlphaNext;
      alphaNext = -(phi0 * alpha ^ 2) / ...
          (2 * (phiAlpha - phi0 - phiFirst0 * alpha));
    else
      c = 1 / ((alpha ^ 2) * (alphaNext ^ 2) * (alphaNext - alpha));
      M = [alpha ^ 2 -(alphaNext ^ 2); -(alpha ^ 3) alphaNext ^ 3];
      v = [phiAlphaNext - phi0 - phiFirst0 * alphaNext; ...
          phiAlpha - phi0 - phiFirst0 * alpha];
      result = c * M * v;
      a = result(1);
      b = result(2);
      alpha = alphaNext;
      phiAlpha = phiAlphaNext;
      alphaNext = (-b + sqrt(b ^ 2 - 3 * a * phiFirst0)) / (3 * a);
    end

    phiAlphaNext = phi(alphaNext);

    if (isSufficientDecrease(alphaNext, c1, phi0, phiFirst0, ...
        phiAlphaNext))
      alpha = alphaNext;
      phiAlpha = phiAlphaNext;      
      return;
    end
    
    i = i + 1;
  end
end

function tf = isSufficientDecrease(alpha, c1, phi0, phiFirst0, ...
    phiAlpha)
  tf = phiAlpha <= (phi0 + c1 * alpha * phiFirst0);
end

function tf = isGoldilocks(alphaNext, alpha)
  r = abs((alphaNext - alpha) / alpha);
  tf = r >= 0.1 && r <= 0.9;
end
