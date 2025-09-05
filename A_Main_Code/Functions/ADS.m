function R = ADS(R,Model,p_)
%__________________________________________________________________________
% R = ADS(R,P,Model)
% Augmented disturbance smoother with time varying system matrices                                  %
% PURPOSE: Returns the smoothed meas eq and transition equation      
%          disturbances under the assumption HG'=0                   
% INPUT:  Zstack  = N*T times m matrix                               
%         Tmstack = T*m times m transition matrix                    
%         Vm      = T*N by (k+d+1) matrix of AKF innovations         
%         Fm      = Covariance matrix of the innnovations            
%         Km      = Kalman gain computed by the AKF                  
% OUTPUT: diffuse smoothed disturbances                              
%         em = e_1|e_2|...|e_t|......|e_T;                           
%         rm = r_0|r_1|...|r_t|......|r_{T-1};                       
%         Dm = D_1|D_2|...|D_t|......|D_T;                           
%         Nm = N_0|N_1|...|N_t|......|N_{T-1};                       
%__________________________________________________________________________
  eval(['S = ' Model '(p_,[],0);'])
  
  [n,m]    = size(S.Z);
  nobs     = size(R.Vm,1);
  R.em     = nan(nobs,n);     
  R.Dm     = nan(nobs,n,n);
  R.rm     = nan(nobs,m);        
  R.Nm     = nan(nobs,m,m);
  
% Initialize  
  Q_t = 0;
  for t = 1:nobs
      V  = squeeze(R.Vm(t,:,:));
      if n == 1 
         V = V';
      end
      Q_t = Q_t + V' * squeeze(R.iF(t,:,:)) * V;
  end

  rQ       =  size(Q_t,1);
  ql       =  Q_t(1,1); 
  sl       =  Q_t(2:rQ,1); 
  Su       =  Q_t(2:rQ,2:rQ);
  
  mse_d    =  inv(Su);
  R.delta  = -inv(Su) * sl; 

  M        = zeros(m,size(R.Vm,3)); 
  N        = 0;
  
% Run smoother
  for t = nobs:-1:1
      
         eval(['S = ' Model '(p_,S,' num2str(t) ');']);
         V  = squeeze(R.Vm(t,:,:));
         iF = squeeze(R.iF(t,:,:));
         K  = squeeze(R.Km(t,:,:));
         
         if n == 1; 
            V = V'; 
            K = K';
         end
         
         E   = iF*V - K'*M;
         D   = iF   + K'*N*K;
         M   = S.Z'*E   + S.T'*M;
         N   = S.Z'*iF*S.Z + (S.T-K*S.Z)'*N*(S.T-K*S.Z);
                  
         R.em(t,:,:)  = E  * [1;R.delta];
         R.rm(t,:)    = M  * [1;R.delta];
         R.Dm(t,:,:)  = D - E(:,2:size(E,2)) * mse_d * E(:,2:size(E,2))';
         R.Nm(t,:,:)  = N - M(:,2:size(M,2)) * mse_d * M(:,2:size(M,2))';
  end
end



