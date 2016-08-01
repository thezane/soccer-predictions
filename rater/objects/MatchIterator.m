classdef MatchIterator
  properties (Access = private)
    mTree
    dates
    I
    N
  end
  
  methods
    function mi = MatchIterator(mTree)
      mi.mTree = mTree;
      mi.dates = sort(keys(mi.mTree)); 
      mi = mi.reset();
    end
    
    function tf = hasNext(mi)
      tf = mi.I(1) < mi.N(1) || mi.I(2) <= mi.N(2);
    end

    function [mi match] = next(mi)
      if (mi.I(2) > mi.N(2))
        mi.I(1) = mi.I(1) + 1;
        mi.I(2) = 1;
        mi.N(2) = length(mi.mTree(mi.dates{mi.I(1)}));
      end
               
      nextDate = mi.dates{mi.I(1)};
      mDateList = mi.mTree(nextDate);
      match = mDateList(mi.I(2));
      mi.I(2) = mi.I(2) + 1;
    end
    
    function mi = reset(mi)
      mi.I = [0 1];
      mi.N = [length(mi.dates) 0];
    end
  end
end 
