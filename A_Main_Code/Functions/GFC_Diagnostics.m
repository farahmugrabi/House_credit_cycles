function D_ = GFC_Diagnostics(Ym,M_,S,p_,fpath,ctry)
%__________________________________________________________________________
% D_ = GFC_Diagnostics(Date,Ym,M_,S,p_)
% Runs diagnostics for the GFC model
%__________________________________________________________________________
 
   n = 3;
   
%__________________________________________________________________________
% Covariances of trend components 
%__________________________________________________________________________
   V_ir  = (S.G(1:n,1:n))           * (S.G(1:n,1:n))';
   V_rw  = (S.H(1:n,n+1:2*n))       * (S.H(1:n,n+1:2*n))';
   V_sl  = (S.H(n+1:2*n,2*n+1:3*n)) * (S.H(n+1:2*n,2*n+1:3*n))';
        
   disp(' ')
   disp(['' kron('_',ones(1,75))])
   disp('Trend components')
   disp(['' kron('_',ones(1,75))])
   disp(' ')
   disp('Standard deviations (*100)');
   disp([' Level     :  [' num2str(sqrt(diag(V_rw))'*100,'%10.4f') ']'])
   disp([' Slope     :  [' num2str(sqrt(diag(V_sl))'*100,'%10.4f') ']'])
   disp([' Irregular :  [' num2str(sqrt(diag(V_ir))'*200,'%10.4f') ']'])

   disp(' ')
   disp('Correlation matrices');
   disp(' ')
   disp('Level')
   disp(num2str(cor_mat(V_rw),'%10.4f'))
   disp(' ') 
   disp('Slope')
   disp(num2str(cor_mat(V_sl),'%10.4f'))
   disp(' ')
   disp('Irregular')
   disp(num2str(cor_mat(V_ir),'%10.4f'))
 
%__________________________________________________________________________
%  Parameter estimates cycle     
%__________________________________________________________________________
   disp(' ')
   disp(['' kron('_',ones(1,75))])
   disp( 'Parameters cyclical components ')
   disp(['' kron('_',ones(1,75))])
      
 % Cycle dynamics 
   rho  =  exp(-p_(1:3).^2);
   len  =  2.0*pi ./ p_(4:6);
   car  = (2* ones(1,3) ./ (ones(1,3)+exp(-p_(7:9)))) - ones(1,3);
   
   disp(' ')
   disp('AR1 roots') 
   disp(num2str(car   ,'%10.3f'))
   disp('Cycle decays') 
   disp(num2str(rho,'%10.3f'))
   disp(' ')
   disp('Cycle lengths (quarters)') 
   disp(num2str(len,'%10.3f'))
   disp(' ')
   disp('Cycle lengths (years)') 
   disp(num2str(len./4,'%10.3f'))
      
 % Co-movements  
   H_k  = S.H(2*n+1:3*n,3*n+1:4*n);
   A    = S.Z(:,4*n+1:5*n);
   A_s  = S.Z(:,5*n+1:6*n);
   
   disp(' ') 
   disp('Corr matrix cyc innovations')
   disp(num2str(cor_mat(H_k*H_k'),'%10.4f'))
   
   disp(' ') 
   disp('Loadings A and A*')
   disp(num2str([A A_s],'%10.4f'))

 % Loadings and phase shifts per factor
   for i = 1:3
       rr(:,i)  = sqrt(A(:,i).^2 + A_s(:,i).^2); 
       xi(:,i) = atan(A_s(:,i) ./ A(:,i));
   end
 
   disp(' ') 
   disp('Phase-adj loadings and phase per cycle')
   disp(num2str([rr xi],'%10.4f'))
   
%__________________________________________________________________________
%  Cross correlation function and spectrum    
%__________________________________________________________________________
   Z_c            =  S.Z(1:n,2*n+1:6*n);
   T_c            =  S.T(2*n+1:6*n,2*n+1:6*n);
   H_c            =  S.H(2*n+1:6*n,3*n+1:5*n);   
   
  [D_.std,D_.CC]  =  CCF_AR1(Z_c,T_c,H_c,eye(3),20,0);
  
   disp(' ') 
   disp('Standard deviations cyclical components x 100')
   disp(num2str([D_.std'].*100,'%10.3f'))
   
   D_.SGF         =  SGF_AR1(Z_c,T_c,H_c,500,fpath,ctry,M_);  
   
 end
