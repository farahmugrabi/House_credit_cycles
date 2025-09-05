function p_ = select(p0,M)
%__________________________________________________________________________
%  function p_ = select(p,M)
%  Selects those parameters from p for which p_x = 1
%  and scales them appropriately
%__________________________________________________________________________
  if sum(M.px) == 0;  
     error('SELECT: No parameters selected');
  end
  p0  = p0 .* M.ps;
  p_  = p0(logical(M.px));
 