x = [10 10]';
tol = 1e-03;
f = @(x) 100 * (x(2)- x(1) ^ 2) ^ 2 + (1 - x(1)) ^ 2;
gradientF = @(x) approxGradient(x, f);
hessianF = @(x, g) approxHessian(x, g, f);
%gradientF = @(x) [-400 * (x(2) - x(1) ^ 2) * x(1) - 2 *(1 - x(1)); 200 * (x(2) - x(1) ^ 2)];
%hessianF = @(x, g) [-400 * (x(2) - 3 * x(1) ^ 2) + 2, -400 * x(1); -400 * x(1), 200];
bfgs(x, f, gradientF, hessianF, tol)
