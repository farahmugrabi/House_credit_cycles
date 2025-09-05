function R = AKF(Ym,Model,p_)
%__________________________________________________________________________
%                   AUGMENTED KALMAN FILTER                          
% PURPOSE: performs the AKF for a time variant SSF                   
% INPUT: Ym  T x N matrix containing the time series 
%                                                                                           %                                                                    %
% OUTPUT: Vm = T*N by (k+d+1) matrix of AKF innovations              
%         Fm = Covariance matrix of the innnovations                 
%         Km = Gain matrix                                           
%                                                                    
%__________________________________________________________________________
   
% Initialization 
  eval(['S = ' Model '(p_,[],0);'])
      
  if size(S.Z,1) ~= size(Ym,2)
      disp(['Data vector : ' num2str(size(Ym,2))]);
      disp(['Z           : ' num2str(size(S.Z,1))]);
      error('Data vector does not fit Z'); 
  end
  
  A   = S.W_0*([S.b0 S.Bu]);     
  P   = S.P_0;
  
  [n,m] = size(S.Z);
  nobs  = size(Ym,1);

  R.Vm  = nan(nobs,n,size(A,2));   
  R.Fm  = nan(nobs,n,n);
  R.Km  = nan(nobs,m,n);  
 
 % Filtering                     
   for t = 1:nobs
   % Obtain SSF   
         eval(['S = ' Model '(p_,S,' num2str(t) ');']);
         [y,Z,G,X,L] = MissData(Ym(t,:)',S);
 
         if isempty(y)
 
           A    =  S.W*([S.b0 S.Bu]) + S.T*A; 
           P    =  S.T*P*S.T' + S.H*S.H';  
           iF   =  zeros(n,n);
           K    =  zeros(m,n);     
           V    =  zeros(n,size(A,2));

         else
           V    =  [(y - X*S.b0) (-X*S.Bu)] - Z*A;
           iF   =  inv(Z*P*Z' + G*G'); 
           K    =  S.T*P*Z'*iF;
         
           A    =  S.W*([S.b0 S.Bu]) + S.T*A + K*V; 
         
           P    = (S.T-K*Z)*P*S.T' + (S.H-K*G)*S.H';
           P    =  0.5*(P + P');
           
         % Restore structure of V, iF and K 
           V    =  L*V;
           iF   =  L*iF*L';
           K    =  K*L';
         end
                  
         R.Vm(t,:,:)  =  V;
         R.iF(t,:,:)  =  iF;
         R.Km(t,:,:)  =  K;
   end
end

