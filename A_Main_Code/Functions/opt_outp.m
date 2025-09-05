function opt_outp(flag,mlogL,p_,cov_,M_,T)
%__________________________________________________________________________
%  opt_outp(mlogL,cov,p_,M_)                                            
%  Summary of Parameters                                             
%  mlogL      : Likelihood                                                            
%  p          : Estimated parameters                               
%  M_         : Parameter starting values, index & scaling                            
%  cov_p      : Hessian  
%
%  Note that cov_p refers to scaled parameters. This needs to be adjusted
%__________________________________________________________________________
   npar  = length(p_);
   nopt  = sum(M_.px);
   ix    = mkindex(M_.px); 
   
 % Akaike info Crit with free parameters 
 % Akaike info crit with all parameters
 % Bayesian info crit
   aic1  =  2 * (-mlogL + nopt);                 
   aic2  =  2 * (-mlogL + npar);                 
   bic   = -2 * mlogL + nopt*log(T);  
 
   disp(' ') 
   disp(['' kron('_',ones(1,75))]);
   disp([' Flag fminunc  : ' num2str( flag,0)])
   disp(['   1  Magnitude of gradient small enough '])
   disp(['   2  Change in X too small'])
   disp(['   3  Change in objective function too small'])
   disp(['   5  Cannot decrease function along search direction'])
   disp(['   0  Too many function evaluations or iterations'])
   disp(['   9  No optimisation'])
   disp([' For other values see help fminunc '])
   disp(' ')
   disp(['' kron('_',ones(1,75))]);
   disp([' Likelihood    : ' num2str( mlogL,'%16.8f')])
   disp([' AIC (all pars): ' num2str( aic2,'%16.8f')])
   disp([' AIC (opt pars): ' num2str( aic1,'%16.8f')])
   disp([' BIC           : ' num2str( bic,'%16.8f')])
   disp(' ')
   disp(['' kron('_',ones(1,75))]);
   disp('Parameters')
   disp('Name         Scale    Startvec       Optvec        STD         T-Val') 
   disp(' ')
   
   for i = 1:npar
       e1  = M_.p0(i)>=0;
       e2  = p_(i)>=0;
       
       s1  = [M_.p_n{i} empty(13-length(M_.p_n{i}))];
       s2  = [num2str(M_.ps(i),4)];
       s2  = [s2 empty(8-length(s2)+e1)];
       
       s3  = [num2str(M_.p0(i),'%16.8f')];
       s3  = [s3 empty(15-length(s3)-e1+e2)];

       if M_.px(i)
          s4   = [num2str(p_(i),'%16.8f')];
          s4   = [s4 empty(15-length(s4)-e2)]; 
       else
          s4   = empty(15); 
       end
       
       if M_.px(i) & ~isnan(cov_)
          j    = find(ix==i); 
          std  = sqrt(cov_(j,j)) / M_.ps(j);
          tv   = p_(j) ./ std;
          
          e1   = tv >= 0;
          e2   = abs(tv)>10;
          s5  = [num2str(std,'%9.4f')];
          s5  = [s5 empty(12+e1-e2-length(s5))];
          
          s6  = [num2str(tv,'%9.4f')];
          disp([s1 s2 s3 s4 s5 s6])  
       else
          disp([s1 s2 s3 s4])
       end
        
   end
   disp(' ')
   disp(['' kron('_',ones(1,75))]);
   %disp('Press Enter to continue')
   %pause
end

function      s = empty(n)
   s = kron(' ',ones(1,n));
end



