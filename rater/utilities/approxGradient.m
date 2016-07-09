function g = approxGradient(x, fX, f, e)
  n = length(x);
  g = zeros(n, 1);
  I = eye(n);
  i = 1;
  
  while (i <= n)
    g(i) = (f(x + e * I(:, i)) - fX) / e;
    i = i + 1;
  end 
end
