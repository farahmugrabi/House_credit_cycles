function p0 = deselect(p1,p0,M)
%__________________________________________________________________________
%  p = deselect(p1,p0,M)
%  Interprets p1 as a subvector of p0 with updated values
%  Rescales parameter vector p1 with scaling factors from M.px and 
%  inserts the rescaled values into p0 for all indices with M.px = 1
%__________________________________________________________________________
  ix      = logical(M.px);
    
  p1      = p1 ./ M.ps(ix);
  p0(ix)  = p1;