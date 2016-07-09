function x = roundDecimals(x, n)
  c = 10 ^ n;
  x = c * x;
  x = round(x);
  x = x ./ c;
end
