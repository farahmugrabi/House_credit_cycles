function R = DKF(Ym,Model,p_)
%______________________________________________________________________
% USAGE R = DKF(Ym,Model,Par)
% Diffuse Kalman filter for time-varying system matrices.
% This does the diffuse filter with full collapsing only
%
% The model is        y_t   = Z(t) * a_t  + X(t)*b + G(t) * eps_t       
%                     a_t+1 = T(t) * a_t  + W(t)*b + H(t) * eps_t       
%
% with H'G = 0  & diagonal matrix G G'                                                         
%
%______________________________________________________________________
% INPUT  
%        Ym         Data                                 (nobs x n)  
%        Model      Name of function to build SSF        (string)
%        Par        Parameter structure to build SSF
%                   including inital conditions    
% OUTPUT 
%        R.Am       Predicted state vector  A_t|t-1      (nobs x m)  
%        R.AmU      Filtered  state vector  A_t|t        (nobs x m)  
%        R.Pm       Predicted covariance of A_t|t-1      (nobs x m x m)  
%        R.PmU      Filtered  covariance of A_t|t        (nobs x m x m)  
%        R.Q        Sum of v'F^{-1}v                                   
%        R.SF       v' inv(F) v
%        R.loglik   Value of likelihood function
%
%        R.AmF      only if R.fcstH is defined     (nobs x m x fcstH+1)
%                   AmF(t,:,h) contains A_t+h-1|t   
%
%______________________________________________________________________
% a) DKF uses a function S = Model(Par,S), which builds the state space
%    form. Par is a structure containing Parameter values and other
%    arbitrary information. Structure S contains matrices {Z, T, G, H}  
%    plus initial conditions A1 and P1 = cov(A1) for the state vector.
%    Use S=[] for the 1st call of Model and update S thereafter.
%    Model is passed to DKF as a text string
%______________________________________________________________________
% Initalise
% Model='GF3_S123_US'; %Farah to obtain objects
% Model='GF3_S23_US_DYN'; %Farah to obtain objects

  eval(['S = ' Model '(p_,[],0);'])
  if size(S.Z,1) ~= size(Ym,2)
      disp(['Data vector : ' num2str(size(Ym,2))]);
      disp(['Z           : ' num2str(size(S.Z,1))]);
      error('Data vector does not fit Z'); 
  end
  
% Output structure & dimensions
  [n,m]  = size(S.Z);
  nobs   = size(Ym,1);
  
  R.sF   =  0;
  R.Q    =  0;
  R.Vm   =  nan(nobs,n);   
  R.Am   =  nan(nobs,m);  
  R.AmU  =  nan(nobs,m);   
  R.Pm   =  nan(nobs,m,m);
  R.PmU  =  nan(nobs,m,m);
  
%______________________________________________________________________
% Filter the first S.ndiff observations  
  A   =  S.W_0*([S.b0 S.Bu]);     
  P   =  S.P_0 ; 
  Q   =  0;
     
  for t = 1:S.nd  
        eval(['S = ' Model '(p_,S,' num2str(t) ');']);
        [y,Z,G,X,L] = MissData(Ym(t,:)',S);
        
         PZ   =  P*Z';
         iF   =  inv(Z*P*Z'+ G*G');
         PZF  =  PZ*iF;
         K    =  S.T*PZF;

         V    = [(y - S.X*S.b0) (-X*S.Bu)] - Z*A;
         Q    =  Q  + V'*iF*V;              
         A    =  S.W*[S.b0 S.Bu] + S.T*A + K*V;       
         
         P    = (S.T-K*Z)*P*S.T' + S.H*S.H';     
   
         R.Am(t,:)    =  A(:,1);  
         R.Pm(t,:,:)  =  P;
         R.iF(t,:,:)  =  iF;
         R.K(t,:,:)   =  K;

         R.sF =  R.sF - log(det(iF)); 
  end

%______________________________________________________________________   
% Collapse
  rQ      = size(Q,1);    
  ql      = Q(1,1);    
  sl      = Q(2:rQ,1);  
  Su      = Q(2:rQ,2:rQ);  
  
  R.Q     = ql - sl'*inv(Su)*sl;
  delta   = -inv(Su)*sl;
  P       = P + A(:,2:size(A,2))*inv(Su)*A(:,2:size(A,2))';
  A       = A*[1;delta];
  beta    = S.b0 + S.Bu*delta;
  R.Su_a  = Su;
  
%______________________________________________________________________   
% Continue with standard Kalman filter
  Au = zeros(size(A));
  
  for t = S.nd+1:nobs
      R.Am(t,:)   = A;
      R.Pm(t,:,:) = P;

    % Obtain SSF 
      eval(['S = ' Model '(p_,S,' num2str(t) ');']);
      [y,Z,G,X,L] = MissData(Ym(t,:)',S);

      if isempty(y)
         Au   = A;
         Pu   = P;
         A    = S.T*A      + S.W*S.b0;
         P    = S.T*P*S.T' + S.H*S.H';
       
         iF   = zeros(n,n);
         K    = zeros(m,n);     
         V    = zeros(n,1);
      
      else
       % Kalman gain
         PZ    = P*Z';
         iF    = inv(Z*PZ + G*G');
         PZF   = PZ*iF;
         K     = S.T*PZF;
         V     = y - Z*A  - X*S.b0;
         
       % A_t|t & P_t|t
         Au    = A  + (PZF * V);           
         Pu    = P  - (PZF * PZ');
             
       % A_t+1|t & P_t+1|t  
         A     =  S.T*A + K*V + S.W*S.b0;
         P     = (S.T-K*Z)*P*S.T' + S.H*S.H';
         P     =  0.5 * (P+P');           
       
       % Likelihood  
         R.Q   =  R.Q  + V'*iF*V;
         R.sF  =  R.sF - log(det(iF)); 
                  
       % Restore structure of V, iF and K 
         V     =  L*V;
         iF    =  L*iF*L';
         K     =  K*L';
      end
         
      V(V==0)        = nan;
      R.Vm(t,:)      =  V;  
      R.AmU(t,:)     =  Au;  
      R.PmU(t,:,:)   =  Pu;
      R.iF(t,:,:)    =  iF;
      R.K(t,:,:)     =  K;
  
    % Produce h steps ahead forecast  
       if isfield(p_,'fcstH') 
          R.AmF(t,:,1) = Au;
          R.AmF(t,:,2) = A;
          for h = 3:Par.fcstH
              R.AmF(t,:,h) = S.T*R.AmF(t,:,h-1) + S.W*S.b0;
          end        
       end    
  end 
  
% Likelihood  
  R.loglik = - 0.5 * (R.sF + R.Q + log(det(R.Su_a)));
