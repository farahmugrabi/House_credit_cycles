function [S_C,COR] = RT_tests(C,Cs,Cf,c1,c2,scale)
%__________________________________________________________________________
% RT_tests(C_true,C_smo,Cf,c1,c2)                                                  
% Diagnostics of cyclical estimates          
% Runs diagnostics of filtered and smoothed estimates 
% INPUT
%    C_true     True cycle                                       [n x 1]
%    C_smo      Two_sided estimate of cycle (t|t+h)              [n x 1]
%    C_filt     Filtered estimate of cycle (t|t)                 [n x 1]
%    c1         Cut-off points: Calculate statistics 
%    c2         from c1:end-c2
%    scale      = 1 -> express statistics in terms of
%                      percentage of std (C)
%                      (with the exception of S_C(6)!)
%               = 0 -> express in levels
%
% OUTPUT
%    S_C(1)  = std(C)     S_C(2)  = std(Cs)     S_C(3)  = std(Cf) 
%    S_C(4)  = std(C-Cs)  S_C(5)  = std(C-Cf)   S_C(6) = std(Cf-Cs)
%
%__________________________________________________________________________
   
% Cut cycles
  C    =  C(c1:c2);
  Cs   = Cs(c1:c2);
  Cf   = Cf(c1:c2); 
  
% Calculate errors  
  Es    = C  - Cs;
  Ef    = C  - Cf;
  Efs   = Cs - Cf;
    
% Standard deviations & erros
% Set scaling
  if scale; S = std_0(C); 
  else      S = 1;   
  end
  
  S_C     = nan(1,6); 
  S_C(1)  = std_0(C);
  S_C(2)  = std_0(Cs)  / S;
  S_C(3)  = std_0(Cf)  / S;
  S_C(4)  = std_0(Es)  / S;
  S_C(5)  = std_0(Ef)  / S;
  S_C(6)  = std_0(Efs) / std_0(Cs);
 
% Correlations
  COR     = nan(1,6); 
 
  c = corr([C  Cs]);  COR(1)  = c(1,2);
  c = corr([C  Cf]);  COR(2)  = c(1,2);
  c = corr([Cs Cf]);  COR(3)  = c(1,2);
  c = corr([C  Es]);  COR(4)  = c(1,2);
  c = corr([C  Ef]);  COR(5)  = c(1,2);
  c = corr([Cs Efs]); COR(6)  = c(1,2);
  
end

function s = std_0(x)
% Calculates uncentered standard deviation around zero
% for vector x
  s  =  sqrt(sum(x.*x) / length(x));
end

