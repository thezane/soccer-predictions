classdef RatingsOptions
  properties
    qK
    tK
    c
    fTree
    fNames
    numFs
    tolRel
    tolScale
  end
  
  methods
    function rOptions = RatingsOptions(fTree, tolRel, tolScale)
      rOptions.tolRel = tolRel;
      rOptions.tolScale = tolScale;
      rOptions.fTree = fTree;
      rOptions.fNames = keys(rOptions.fTree);
      rOptions.numFs = length(rOptions.fNames);
    end
    
    function rOptions = update(rOptions, qK, tK, c, strFs)
      rOptions.qK = qK;
      rOptions.tK = tK;
      rOptions.c = c;
      i = 1;
      
      while (i <= rOptions.numFs)
        strF = strFs(i);
        rOptions.fTree(rOptions.fNames{i}) = [strF 1 / strF];
        i = i + 1;
      end
    end
  end
end
