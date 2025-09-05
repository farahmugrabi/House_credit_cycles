function S = GF3_S23_US_DYN(p,S,t)
%__________________________________________________________________________
% function S = GECP3(p) 
% THIS VERSION IS ADAPTED TO RUN WITH DYNARE OUTPUT
% A FEW PARAMETER TRANSFORMS ARE SIMPLY NOT NEEDED
%
% - INPUT: (scaled) parameters (p)                                
% - OUTPUT: State space form S                                     
%
% The state is: trend, slope cycle, cycle*  each being n x 1                              
%__________________________________________________________________________
% Parameter indices regression effects (dummies) 
  rx   = [55 56 57];                      

% Below there are 2 examples
  if ~isempty(S)
      S.X  =  zeros(size(S.X));
      S.W  =  zeros(size(S.W));
     
    % Dummies HPR 
      if t == 15;  S.X(3,1) =  1;  end
      if t == 16;  S.W(3,2) =  1;  end
  
    % Step-dummy credit
      if t == 29;  S.W(2,3) = 1;  end  
      
      return
  end    
  
%__________________________________________________________________________
% State space matrices      
%__________________________________________________________________________
  n    =  3;
  S.Z  =  zeros(  n,6*n);
  S.T  =  zeros(6*n,6*n);
  S.G  =  zeros(  n,5*n);
  S.H  =  zeros(6*n,5*n);
  
% ______________________________________
% Indices cyclical components and shocks
  xP    = 2*n+1:4*n;
  xC    = 4*n+1:6*n;
  xS    = 2*n+1:6*n;
  xk    = 3*n+1:5*n;

% Similar cycles restrictions
  p(3)  = p(2);
  p(6)  = p(5);
  p(9)  = p(8);

% Restrictions on loadings  
  p(32) = 0;
  p(42) = 0;
  p(48) = 0;
  
% Dynamics  
  RC          =  p(1:3)  .* cos(p(4:6));
  RS          =  p(1:3)  .* sin(p(4:6));
  TC          = [diag(RC) diag(RS); diag(-RS) diag(RC)];
  CAR1        =  diag(p(7:9));

% Correlation matrix cyc innovations
  C           = ld_mat(eye(n),p(46:48));
  
% Loadings 
  A           = reshape(p(30:38)             ,3,3)';
  A_s         = reshape([zeros(1,3) p(40:45)],3,3)';

  S.T(xP,xP)  =  TC; 
  S.H(xP,xk)  =  kron(eye(2),C);
  S.T(xC,xP)  =  eye(6);
  S.T(xC,xC)  =  kron(eye(2),CAR1);
  
  S.Z(1:n,4*n+1:5*n)  =  A;
  S.Z(1:n,5*n+1:6*n)  =  A_s;
  
% ______________________________________
% Irregular  
  S.G(1:n,1:n)           =  diag(p(50:52));   
   
% ______________________________________
% Trends                   
  S.Z(1:n,1:n)           =  eye(n);
  S.T(1:2*n,1:2*n)       =  kron([1 1;0 1],eye(n));

  x1  = diag([p(10) p(11) p(12)]);
  x2  = diag([p(20) p(21) p(22)]);

  X1  = [1   p(13) p(14); p(13) 1  p(15); p(14) p(15)  1];  
  X2  = [1   p(23) p(24); p(23) 1  p(25); p(24) p(25)  1];
  
  S.H(1:n,n+1:2*n)       =  chol(x1*X1*x1)';
  S.H(n+1:2*n,2*n+1:3*n) =  chol(x2*X2*x2)';    
 
  
% ______________________________________
% Initial conditions 

% Non-stationary elements of state vector
  a0  = [ones(1,2*n) zeros(1,4*n)]; 
  
% Initial covmat stationary part 
  S.P_0        = zeros(6*n,6*n);
  S.P_0(xS,xS) = InitCov(S.T(xS,xS),S.H(xS,xk)*(S.H(xS,xk))');
                       
% Initial conditions non-stationary part                          
  S.nd   = sum(a0);
  n      = size(S.Z,1);
  m      = size(S.T,1);
  
  S.X    = zeros(n,S.nd);
  S.W    = zeros(size(S.T,1),S.nd);
  S.Bu   = eye(S.nd);      
  S.b0   = zeros(S.nd,1);              
   
  ax     = mkindex(a0);
  S.W_0  = zeros(m,length(ax));
  for  i = 1: length(ax)
      S.W_0(i,ax(i)) = 1; 
  end
  
  S.nd   =  sum(a0);
  n      = size(S.Z,1);
  m      = size(S.T,1);
 
%__________________________________________________________________________
% Regression effects                                  
   nreg    = length(rx);
   if nreg > 0;
      S.X    = [zeros(size(S.X,1),nreg)   S.X  ];    
      S.W    = [zeros(size(S.W,1),nreg)   S.W  ];
      S.W_0  = [zeros(size(S.W,1),nreg)   S.W_0]; 
      S.Bu   = [zeros(nreg,size(S.Bu,2));   S.Bu];
      S.b0   = [p(rx)'; S.b0];
   end
      
end
