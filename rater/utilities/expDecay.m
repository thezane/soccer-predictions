function y = expDecay(t, k, y0)
  y = y0 .* exp(-k * t);
end 
