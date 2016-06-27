a = [1 1]';
d = [1 1]';
nu = 1e-06;
alpha = 1;
lambda = 1;
i = 0;

while (i <= 7)
  A = [0 2 * log(i + 1); 0 0];
  [a d] = computeStr(A, a, d, alphas, nu, lambda);
  log([a d]')
  pause
  i = i + 1;
end 
