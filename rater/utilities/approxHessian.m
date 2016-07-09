function B = approxHessian(x, g, f, e)
  n = length(x);
  B = zeros(n, n);
  I = eye(n);
  i = 1;
  
  while (i <= n)
    xe = x + e * I(:, i);
    fXe = f(xe);
    B(i, :) = (approxGradient(xe, fXe, f, e) - g(i)) / e;
    i = i + 1;
  end
  
  B = (B + B') / 2;
end
