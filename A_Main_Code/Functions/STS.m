function R = STS(R,Model,p_)
%__________________________________________________________________________
% R = State_smoother(R,P,Model)
% State smoother for time varying system matrices                         
% PURPOSE: Returns the full sample estimates of the states          
%          based on the disturbance smoother                        
% INPUT:                                                            
%          em, rm, smoothed residuals produced by the diffuse       
%                  disturbance smoother                             
% OUTPUT: am = a_1|a_2|............|a_T                             
%         signal = Z * a + B * X                                    
%__________________________________________________________________________
%proc (2)=TvStS(Zstack,Xstack,Gstack,Tmstack,Wstack,
%               Hstack,bl,Bu,W_0,P_0,em,rm,delta);

     eval(['S = ' Model '(p_,[],0);'])
     
     [n,m]     = size(S.Z);
     nobs      = size(R.Vm,1);
     R.Am      = nan(nobs,m);
     R.signal  = nan(nobs,n);
          
     beta      = S.b0+S.Bu*R.delta;
     a         = S.W_0*beta + S.P_0 * R.rm(1,:)';
     R.Am(1,:) = a;

     for t = 2:nobs
        eval(['S = ' Model '(p_,S,' num2str(t) ');']); 
 
        a  = S.T * a + S.W * beta + S.H*S.H' * R.rm(t,:)';
        R.Am(t,:)     = a;
        R.signal(t,:) = S.Z * a + S.X * beta;
      end



