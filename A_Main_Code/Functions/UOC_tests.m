function D_ = UOC_tests(Date,Ym,R,d)
%__________________________________________________________________________
% UOC_tests(Ym,R,d)                                                  
% Diagnostics of prediction errors: Whiteness, Normality, AIC          
% The procedure assumes full collapsing after ndiff obs              
% INPUT  : R.Vm   : Prediction errors                     [T x n]         
%          R.iF   : inv(F) covariance of Vm               [T x n x n]                                     
%          Ym     : Data                                  [T x n]                                      
%          d      : Nr of obs to drop at beginning    
%
% The treatment of missing values is a bit tricky 
% Elements of R.Vm and R.iF are set to zero for missing
%__________________________________________________________________________
  format short
  
  disp(' ');
  disp(['' kron('_',ones(1,75))])
  disp('Standardized prediction errors ');
  disp(['' kron('_',ones(1,75))])
  disp(' '); 

% Retrieve V  & iF
  [nobs,n]  =  size(R.Vm); 
  Cm        =  nan(nobs,n); 
   for t = d+1:nobs 
      iF       =  squeeze(R.iF(t,:,:));
      ix       =  ~isnan(Ym(t,:));
      F        =  inv(iF(ix,ix));
      Cm(t,ix) =  diag(F); 
  end

% Drop 1st d observations
  Vm       =  R.Vm(d+1:end,:);
  Cm       =    Cm(d+1:end,:);
  Ym       =    Ym(d+1:end,:); 
  Date     = Date(d+1:end,:);
 [nobs,n]  =  size(Vm); 
 
  
  for i = 1:n
    %________________________________________________________________
    % Prediction errors
    % v_0/_N set missing to 0 and nan
      F_            = Cm(:,i); 
      v_0           = Vm(:,i);
      v_N           = v_0;
      v_N(v_0==0)   = nan;
     
    % Standardise  
      vs_N          = (v_N  - nanmean(v_N)) ./ sqrt(F_);
           
      n_i     =  nobs - sum(isnan(Ym(:,i)));           
      dy_     =  diff(Ym(:,i));
      sv_i    =  nanstd(v_N);  
      sy_i    =  nanstd(dy_);
      rmse    =  sqrt((v_0'*v_0) / n_i);
           
      
    %________________________________________________________________
    % Basic stat prediction errors
      D_.std_vF  = sqrt(F_(nobs))*100;
      D_.std_vs  = sv_i*100;
      D_.rmse(i) = rmse*100;
      D_.R2(i)   = 1 - (sv_i/sy_i)^2;
      
      disp(['' kron('_',ones(1,35))]);
      disp(['Series Nr ' num2str(i)])
      disp(['' kron('_',ones(1,35))]);
      disp([' Stdv Vi *100 (from F) = ' num2str(D_.std_vF,6)]) 
      disp([' Stdv Vi *100 (direct) = ' num2str(D_.std_vs,6)])
      disp([' RMSE                  = ' num2str(D_.rmse(i),6)])
      
      disp([' Mean                  = ' num2str(nanmean(v_N)*100,6)])
      disp([' Stdv mean             = ' num2str(sv_i/sqrt(n_i)*100,6)])
      disp(' ')
      disp([' StDv dy               = ' num2str(nanstd(dy_)*100,6)])
      disp([' RD-Square             = ' num2str(D_.R2(i),'%10.3f')])
      
    %________________________________________________________________
    % Q statistics 
      [qs,pq,scor] = Q_stat(vs_N);
    
      disp(' ');
      disp('Autocorrelations');
      disp(scor(1:6,:)')
      disp('Ljung Box Statistics');
      disp([' Q( 4)    = ' num2str(qs(1),'%10.3f') '  ('  num2str(pq(1),'%10.3f') ')' ])
      disp([' Q( 8)    = ' num2str(qs(2),'%10.3f') '  ('  num2str(pq(2),'%10.3f') ')' ])
      disp([' Q(12)    = ' num2str(qs(3),'%10.3f') '  ('  num2str(pq(3),'%10.3f') ')' ])
      disp([' Q(20)    = ' num2str(qs(5),'%10.3f') '  ('  num2str(pq(5),'%10.3f') ')' ])
      disp([' Q(36)    = ' num2str(qs(9),'%10.3f') '  ('  num2str(pq(9),'%10.3f') ')' ])
      disp(' ')
      
      D_.acf     = scor;
      
      D_.Q04(i)  = qs(1);
      D_.Q12(i)  = qs(3);
      D_.Q20(i)  = qs(5);
     
      D_.Q04p(i) = 1 - pq(1);
      D_.Q12p(i) = 1 - pq(3);
      D_.Q20p(i) = 1 - pq(5);
      
    %________________________________________________________________
    % Jarque Bera test for normality  
      skewness =  nansum(vs_N.^3)  / (nanstd(vs_N)^3 * n_i);
      kurtosis =  nansum(vs_N.^4)  / (nanstd(vs_N)^4 * n_i);
      jb       = (nobs/6) * skewness^2 + (nobs/24) * (kurtosis-3)^2;

      disp(['Skewness    = ' num2str(skewness,6)])
      disp(['Kurtosis    = ' num2str(kurtosis,6)])
      disp(['Jarque Bera = ' num2str(jb,6)])  
      disp(['          p = ' num2str(cdf('Chi2',jb,2),2)])
      disp(' ')

      D_.JB(i)   = jb;
      D_.JBp(i)  = cdf('Chi2',jb,2);
     
    %________________________________________________________________
    % Outliers     
      opt_OL.c  = 5;
      [Xi, Ji]  = Outliers(vs_N,0,opt_OL);
          
      disp('Outliers in prediction errors')
      ix = find(Ji == 1);
      for m = 1:length(ix)
          disp(num2str([ix(m)+d Date(ix(m),:) vs_N(ix(m))]))
      end
      
    %________________________________________________________________
    % Cusum test 
    % Critical values are .850 (10%) and .948 (5%)
      cusum = nan(nobs,1); 
      cusup = nan(nobs,1); 
      cuslo = nan(nobs,1); 
      
      cusum(1) = vs_N(1);
      for j = 2:nobs
         cusum(j) = cusum(j-1) + vs_N(j);
         cusup(j) = .850 * sqrt(nobs) + 2 * 0.850 * j / sqrt(nobs);
         cuslo(j) = -cusup(j);            
      end
      
%       Datp  = Date(:,1) + (Date(:,2)-1) / 4;
%       subplot(n,1,i)
%       plot(Datp(d+1:nobs),[cusum cusup cuslo]);
%       title(['CUSUM test series ' int2str(i)]); 
  end 

end

