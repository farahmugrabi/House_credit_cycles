function [qs,pq,scor] = Q_stat(v)
%__________________________________________________________________________
% Q_ = Q_stat(Date,R,d)                                                  
% Calculates Q statistics of prediction errors 
%               
% INPUT  : Vm     : T x 1 vector of prediction errors             
%          d      : Nr of obs to drop at beginning    
%
% OUTPUT   qs:    : Q stat: qs(k) contains Q stat for 4*k
%          pq     : corresping probabaility values from Chi2(4*k)
%
% Autocorrelations are calculated from my own function acf 
%__________________________________________________________________________

% Standardise v  
  v  =  (v - nanmean(v)) ./ nanstd(v);
         
% Q statistics  
  nobs   =  size(v,1); 
  scor   =  acf(v,36,0)';
   
  csq    =  scor .* scor;
  qs     =  nan(9,1);
  pq     =  nan(9,1);
    
  for j = 1:9
      k      = 4*j; 
      base   = ones(1,k) ./ (nobs-1:-1:nobs-k);
      qs(j)  = nobs*(nobs+2) * (base * csq(2:k+1));
      pq(j)  = cdf('Chi2',qs(j),k);
  end
       
 



