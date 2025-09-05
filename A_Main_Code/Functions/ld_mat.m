function  X = ld_mat(X,p)
%__________________________________________________________________________
% X2 = ld_mat(X,p)
%
% Creates matrix X2 such that X2*X2' is a covariance matrix with standard
% deviations X and correlations as from p
%
% Forms a lower triangular matrix X2 by inserting vector p into the lower
% part of diagonal matrix X. Any off-diagonal elements of X are ignored
% (that is, set to zero).
%
% X is filled up by row [(2,1), (3,1), (3,2), (4,1), ....]
% The purpose is to build up lower triangular matrices for a covariance 
% matrix V = X2*X2' such that the diagonal of X represents standard 
% deviations and p represents correlations
%
% INPUT
%   X             diagional matrix n        x n
%   p                              n(n-1)/2 x 1 
%__________________________________________________________________________

% % Eliminate off-diagonal elements
%   X = diag(diag(X));
%   
% % Check size of p  
%   n = size(X,1); 
%   if length(p) ~= n*(n-1)/2
%       error('Par vector must be n(n-1)/2')
%   end
% 
%  % Insert 
%    k     = 1;
%    for i = 2:n  
%    for j = 1:i-1; 
%        X(i,j) = p(k);
%        k      = k + 1;
%    end
%    end


% Check size of p  
  n = size(X,1); 
  if length(p) ~= n*(n-1)/2
      error('Par vector must be n(n-1)/2')
  end
  
% Create C 
% This is a lower triangular of correlation matrix
  C   = eye(n);
  k   = 1;
  for i = 2:n  
  for j = 1:i-1; 
      C(i,j) = p(k);
      k      = k + 1;
  end
  end
  
  C0  = kron(ones(1,n),sqrt(sum(C.^2,2)));
  C   = C ./ C0;
  
% Std dev * C
  X = diag(diag(X)) * C;
  
end

