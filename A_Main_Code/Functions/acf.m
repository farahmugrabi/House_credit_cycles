function c = acf(y,k,dropflag)
%__________________________________________________________________________
% c = acf(y)                                                  
% Calculates autocorrelations of time series y        
% INPUT  : y      : N x 1 vector of time series             
%          k      : nr of autocorrelations to be computed 
%          drop   : = 1 if initial observations shall be dropped 
%                       to get a balanced sample
% OUTPUT   c      : k x 1 vector of autocorrelations  
%
% The function deals with missing data.
%__________________________________________________________________________

% Check dimensions
  if size(y,1) == 1
     y = y';
  end
  if size(y,2) > 1
     error('Function acf is defined only for single series')
  end
  
% Create lagmatrix
  Y = lagmatrix(y,0:k);
  
  if dropflag
     y = y(k+1:end);
     Y = Y(k+1:end,:);
  end
  n = length(y);

% Standardise 
  m = nanmean(y);
  s = nanstd(y,1);
  y = (y - m) ./ s;

  Y = Y -  (m  * ones(size(Y)));
  Y = Y ./ (s  * ones(size(Y)));
  
% Calculate correlation
  c = nan(k+1,1);
  for i = 1:k+1
      n_   = n - sum(isnan(y)|isnan(Y(:,i)));
      c(i) = nansum(y .* Y(:,i)) ./ (n_  + i-1);
  end
  
  c  = c';
  
end

