function g = approxGradient(x, f)
  e = 1e-06;
  n = length(x);
  g = zeros(n, 1);
  I = eye(n);
  i = 1;
  
  while (i <= n)
    g(i) = (f(x + e * I(:, i)) - f(x - e * I(:, i))) / (2 * e);
    i = i + 1;
  end 
end
