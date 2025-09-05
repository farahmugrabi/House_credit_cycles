function  mlogL = DKFLoglik(p_,Ym,M)
%______________________________________________________________________   
%    mlogL = DKFLoglik(p_,Ym,L)
%    Calculates likelihood for OPTMUM utility                               
%    - INPUT:  p_           Parameters (optimised set)
%              Ym           T x n data matrix
%              L            Structure with additional information
%    - OUTPUT: MLogL        Likelihood                                        
%______________________________________________________________________   
   p     =  deselect(p_,M.p0,M);
   %p= deselect(p_,M_.p0,M_); %Farah to obtain objects
   R     =  DKF(Ym,M.Model,p);
   mlogL =  -R.loglik; 
end
