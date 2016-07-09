function x = trustRegion(x, f, gradientF, hessianF, tol)
  deltaHat = 1;
  nu = 1e-06;
  delta = deltaHat / 2;
  n = length(x);
  z = zeros(n, 1);
  fx = f(x);
  g = gradientF(x, fx);
  B = hessianF(x, g);
  BInv = inv(B);
  pB = -BInv * g;
  mz = model(fx, g, B, z);
  n = length(x);
  format = char(strcat({'%d    '}, ...
      {repmat('%0.4f    ', 1, 2 * n + 1)}));
  i = 1;

  while (norm(g) > tol)
    display(sprintf(format, i, fx, g, x));
    [p mp] = dogLeg(fx, g, B, delta, pB);
    fxp = f(x + p);
    roe = (fx - fxp) / (mz - mp);

    if (roe < 0.25)
      delta = delta / 4;
    elseif (roe > 0.75 && norm(p) == delta)
      delta = min(2 * delta, deltaHat);
    end

    if (roe > nu)
      x = x + p;
      fx = fxp;
      gNext = gradientF(x, fx);
      BInv = updateHessianInv(BInv, p, gNext, g);
      g = gNext;
      B = inv(BInv);
      pB = -BInv * g;
      mz = model(fx, g, B, z);
    end
    
    i = i + 1;
  end
end

function BInv = updateHessianInv(BInv, p, gNext, g)
  s = p;
  y = gNext - g;
  roe = 1 / (y' * s);
  BInvNext = BInv - roe * ((BInv * y) * s' + s * (y' * BInv));
  BInvNext = BInvNext + s * ((roe ^ 2)  * y' * BInv * y + roe) * s';
  BInv = BInvNext;
end

function [p mp] = dogLeg(fx, g, B, delta, pB)
  normPB = norm(pB);

  if (normPB <= delta)
    p = pB;
    mp = model(fx, g, B, p);
    return;
   end

  pU = -((g' * g) / (g' * B * g)) * g;
  normPU = norm(pU);

  if (normPU >= delta)
    p = -delta * g / norm(g);
    mp = model(fx, g, B, p);
    return;
   end

  [a b c] = findQuadraticCoefficients(delta, normPB, normPU, pB, pU);
  discriminant = sqrt(b ^ 2 - 4 * a * c);
  taus = (-b + [discriminant; -discriminant]) / (2 * a);
  ps = [trajectory(pB, pU, taus(1)) trajectory(pB, pU, taus(2))];
  ms = [model(fx, g, B, ps(:, 1)) model(fx, g, B, ps(:, 2))]';

  if (ms(1) <= ms(2))
    I = 1;
  else
    I = 2;
  end

  p = ps(:, I);
  mp = ms(I);
end

function [m n] = model(fx, g, B, p)
  m = fx + g' * p + 0.5 * p' * B * p;
  n = norm(p);
end

function [a b c] = findQuadraticCoefficients(delta, ...
    normPB, normPU, pB, pU)
  a0 = norm(pB - pU) ^ 2;
  b0 = 2 * (pU' * (pB - pU));
  c0 = normPU ^ 2 - delta ^ 2;
  a = a0;
  b = b0 - 2 * a0;
  c = a0 - b0 + c0;
end

function p = trajectory(pB, pU, tau)
  if (0 <= tau && tau <= 1)
    p = tau * pU;
  elseif (1 <= tau && tau <= 2)
    p = pU + (tau - 1) * (pB - pU);
  else
    p = zeros(2, 1);
 end
end
