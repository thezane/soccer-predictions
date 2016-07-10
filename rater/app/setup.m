warning('off', 'MATLAB:table:ModifiedVarnames');
dataPath = '../../data/';
reqPaths = {...
    dataPath, ...
    'gls', ...
    'mcs', ...
    'minq5', ...
    '../utilities', ...
    '../objects', ...
    '../reader', ...
    '../verifier', ...
    '../writer'};
n = length(reqPaths);
i = 1;

while (i <= n)
  addpath(genpath(fullfile(pwd, reqPaths{i})));
  i = i + 1;
end
