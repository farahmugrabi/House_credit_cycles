function [sx,Cx] = CCF_AR1(Z,T,H,Scale,s_max,flag)
%__________________________________________________________________________
% [sx,Cx] = GEC_ccf(Z,T,H)
%  Calculates the autocovariance function of the stochastic process x(t)   
%                    x(t)  = Z w(t)
%                    w(t)  = T w(t-1) + H e(t)    e(t) ~N(0,eye(n))  
%  This is used for the non-similar cycles model
%
%  INPUTS
%   Z             (n x k)
%   T             (k x k)
%   H             (k x k)
%   Scale         (n x 1)              Diagonal matrix with multiplicative
%                                      factor for cycle standard deviations
%   s_max          integer             Order of ACF (t,t-s) , s <= s_max
%   flag           0/1                 = 1 print CCF output
%
% OUTPUTS
%    sx           (n x 1)              Standard deviations of x
%    Cx          ((s_max+1) x n x n)   Cross correlations  of x
%                                      Cx(1,:,:) contains lag zero 
%_____________________________________________________________________

% Definitions
  [n,k]   = size(Z);
  Vw      = nan(s_max+1,k,k);
  Cw      = nan(s_max+1,k,k);
  Vx      = nan(s_max+1,n,n);
  Cx      = nan(s_max+1,n,n);
  
% Yule-Walker equations for w
  Vw(1,:,:) = InitCov(T,H*H');
  for s = 2:s_max+1
      Vw(s,:,:) = T * squeeze(Vw(s-1,:,:));
  end
    
% Cross covariances for x  
  for s = 1:s_max+1
      Vx(s,:,:) = Z * squeeze(Vw(s,:,:)) * Z';
  end

% Auto correlations
  sx    = sqrt(diag(squeeze(Vx(1,:,:))));
  sw    = sqrt(diag(squeeze(Vw(1,:,:))));
  Sx    = sx * sx'; 
  Sw    = sw * sw'; 
 
  for s = 1:s_max+1
      Cw(s,:,:)  = squeeze(Vw(s,:,:)) ./ Sw;
      Cx(s,:,:)  = squeeze(Vx(s,:,:)) ./ Sx;
  end
  
% Display  
  if flag 
     disp(' '); 
     disp('Standard deviations Cycles * 100');
     disp(num2str((100*Scale*sx)','%10.4f'))
 
     disp(' '); 
     disp('Cross correlation function');
     for s = 1:s_max+1
         disp(['Lag ' num2str(s-1,'%5.0f')])
         disp(num2str(squeeze(Cx(s,:,:)),'%10.4f'))
     end
  end
 sx  = Scale * sx;
  
  