function B = approxHessian(x, g, f)
  e = 1e-06;
  n = length(x);
  B = zeros(n, n);
  I = eye(n);
  i = 1;
  
  while (i <= n)
    B(i, :) = (approxGradient(x + e * I(:, i), f) - g(i)) / e;
    i = i + 1;
  end
  
  B = (B + B') / 2;
end
