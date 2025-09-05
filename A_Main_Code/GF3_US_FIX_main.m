%__________________________________________________________________________
% MAIN PROGRAM FOR THE DIFFUSE TIME VARIANT KALMAN-FILTER         
% Estimates the stacked univariate models for US GDP/CTR/HPR.
% Slopes for CTR and HPR restricted to .001. No similar cycles resrictions  
%__________________________________________________________________________
clear
clc
Base_Dir = '\\Filescluster\shared\MPS\WORD\MPFS DEPT\Farah\Alternative_Gap\New_Model\Ruenstler_and_Vlekke_2018\A_Main_Code';
addpath(genpath(Base_Dir));
%__________________________________________________________________________
name_results="baseline_univar";  
ctry  = 'IE';    
   data=load([Base_Dir '\data_model.csv']);
   Date = data(:,1:4);
   X=data(:,2:4);
   
  Ym       = X(13:end,1:3);
  Date     = Date(13:end,1:3);
  Label{1} = [ctry,': GDP'];
  Label{2} = [ctry,': CTR'];
  Label{3} = [ctry,': HPR'];
  
   
%__________________________________________________________________________
% Model
% p(40) - p(42) contain the phase shifts xi(2) ... xi(n)
% p(43) - p(45) contain the elements of X where D =  (I-X)^(1) (I+X).
%               and X is skew-symmetric
% Given that the 1st row/col of D is unity, the same applies to X
%__________________________________________________________________________
  M_.Model   =  'GF3_US';
  
  M_.Trend   =  1:3;
  M_.Slope   =  4:6;
  M_.Cycle   =  13:18;
   
  %__________________________________________________________________________
% TEMPLATE starting values general model 
%__________________________________________________________________________
  M_.p0     =  zeros(1,60);
  M_.ps     =   ones(1,60);
  
% Parameter names 
% Cyc dynamics
  M_.p_n{1}  = 'rho1' ; M_.p_n{2}  = 'rho2';  M_.p_n{3}  = 'rho3';  
  M_.p_n{4}  = 'lam1' ; M_.p_n{5}  = 'lam2' ; M_.p_n{6}  = 'lam3';
  M_.p_n{7}  = 'car1' ; M_.p_n{8}  = 'car2' ; M_.p_n{9}  = 'car3';
    
% Trends and slope  
  M_.p_n{10} = 'RW1 ' ; M_.p_n{11} = 'RW2 ' ; M_.p_n{12} = 'RW3' ; 
  M_.p_n{14} = 'RW21' ; M_.p_n{15} = 'RW31' ; M_.p_n{16} = 'RW32'; 
 
  M_.p_n{20} = 'SL1 ' ; M_.p_n{21} = 'SL2 ' ; M_.p_n{22} = 'SL3 '; 
  M_.p_n{24} = 'SL21' ; M_.p_n{25} = 'SL31' ; M_.p_n{26} = 'SL32'; 
 
% Factor loadings  
  M_.p_n{30} = 'A11 ' ; M_.p_n{31} = 'A12 ' ; M_.p_n{32} = 'A13 ';
  M_.p_n{33} = 'A21 ' ; M_.p_n{34} = 'A22 ' ; M_.p_n{35} = 'A23 '; 
  M_.p_n{36} = 'A31 ' ; M_.p_n{37} = 'A32 ' ; M_.p_n{38} = 'A33 ';

  M_.p_n{40} = 'A*21' ; M_.p_n{41} = 'A*22' ; M_.p_n{42} = 'A*23';
  M_.p_n{43} = 'A*31' ; M_.p_n{44} = 'A*32' ; M_.p_n{45} = 'A*33'; 
  
% Corr cycles  
  M_.p_n{46} = 'C12 ' ; M_.p_n{47} = 'C21 ' ; M_.p_n{48} = 'C23 ';

% Irreg  
  M_.p_n{50} = 'eps1' ; M_.p_n{51} = 'eps2' ; M_.p_n{52} = 'eps3' ;
  
  M_.p_n{55} = 'dum1' ; M_.p_n{56} = 'dum2' ; M_.p_n{57} = 'dum3';
  M_.p_n{58} = 'dum4' ; M_.p_n{59} = 'dum5' ; M_.p_n{60} = 'dum6';
  

%__________________________________________________________________________
%  STACKED UNIVARIATE 
%  NO RANDOM WALK
%  CTR AND HOUSE PRICE SLOPE RESTRICTED
%  NO CAR
%  Likhood  = 2199.73037342
%__________________________________________________________________________
%   M_.p0     =  zeros(1,60);
%   M_.px     =  zeros(1,60);
%   M_.ps     =   ones(1,60);
%   M_.p0(1)  =  0.218521766491;      M_.ps(1)  = 1;         M_.px(1)  = 1;      
%   M_.p0(2)  =  0.079269400908;      M_.ps(2)  = 1;         M_.px(2)  = 1;      
%   M_.p0(3)  =  0.071175614672;      M_.ps(3)  = 1;         M_.px(3)  = 1;      
%   M_.p0(4)  = -0.196225314291;      M_.ps(4)  = 1;         M_.px(4)  = 1;      
%   M_.p0(5)  =  0.199872761747;      M_.ps(5)  = 1;         M_.px(5)  = 1;      
%   M_.p0(6)  =  0.147149738481;      M_.ps(6)  = 1;         M_.px(6)  = 1;      
%   M_.p0(20) =  0.000531367697;      M_.ps(20) = 100;       M_.px(20) = 0;      
%   M_.p0(21) =  0.001000000000;      M_.ps(21) = 1000;      M_.px(21) = 0;      
%   M_.p0(22) =  0.001000000000;      M_.ps(22) = 1000;      M_.px(22) = 0;      
%   M_.p0(30) =  0.006483047530;      M_.ps(30) = 100;       M_.px(30) = 1;      
%   M_.p0(34) =  0.003314596259;      M_.ps(34) = 100;       M_.px(34) = 1;      
%   M_.p0(38) =  0.008897868585;      M_.ps(38) = 100;       M_.px(38) = 1;      
%   M_.p0(55) =  0.008406754205;      M_.ps(55) = 1;         M_.px(55) = 1;      
%   M_.p0(56) =  0.002511484218;      M_.ps(56) = 1;         M_.px(56) = 1;      
%   M_.p0(57) = -0.018715386987;      M_.ps(57) = 1;         M_.px(57) = 1; 

%__________________________________________________________________________
%  STACKED UNIVARIATE
%  NO RANDOM WALK
%  CTR AND HOUSE PRICE SLOPE RESTRICTED
%  WITH CAR
%  Likhood  = 2258.02095568
%
%  LR TEST against all CAR = 0
%  LR = 2*(2258.02-2199.730)= 116.58
%  df = 3, p = 0
%
%  LR test against GF3_S123
%  LR = 2*(2258.0209-2239.1228) =  37.7962
%  df = 6, p =  1.2313e-06
%
%  LR test against GF3_S23
%  LR = 2*(2258.0209-2257.4150) =  1.2118
%  df = 3, p = 0.7502
%__________________________________________________________________________
%   M_.p0     =  zeros(1,60);
%   M_.px     =  zeros(1,60);
%   M_.ps     =   ones(1,60);
%   M_.p0(1)  =  0.246909223075;      M_.ps(1)  = 1;         M_.px(1)  = 1;      
%   M_.p0(2)  =  0.134469584586;      M_.ps(2)  = 1;         M_.px(2)  = 1;      
%   M_.p0(3)  =  0.210005438625;      M_.ps(3)  = 1;         M_.px(3)  = 1;      
%   M_.p0(4)  = -0.171714479451;      M_.ps(4)  = 1;         M_.px(4)  = 1;      
%   M_.p0(5)  =  0.102450016445;      M_.ps(5)  = 1;         M_.px(5)  = 1;      
%   M_.p0(6)  =  0.103823684176;      M_.ps(6)  = 1;         M_.px(6)  = 1;      
%   M_.p0(7)  =  0.332179247959;      M_.ps(7)  = 1;         M_.px(7)  = 1;      
%   M_.p0(8)  =  1.703221850956;      M_.ps(8)  = 1;         M_.px(8)  = 1;      
%   M_.p0(9)  =  2.403849760194;      M_.ps(9)  = 1;         M_.px(9)  = 1;      
%   M_.p0(20) =  0.000449854464;      M_.ps(20) = 1000;      M_.px(20) = 1;      
%   M_.p0(21) =  0.001000000000;      M_.ps(21) = 1000;      M_.px(21) = 0;      
%   M_.p0(22) =  0.001000000000;      M_.ps(22) = 1000;      M_.px(22) = 0;      
%   M_.p0(30) =  0.006631374104;      M_.ps(30) = 100;       M_.px(30) = 1;      
%   M_.p0(34) =  0.004013987449;      M_.ps(34) = 100;       M_.px(34) = 1;      
%   M_.p0(38) =  0.007555021397;      M_.ps(38) = 100;       M_.px(38) = 1;      
%   M_.p0(55) =  0.014428117106;      M_.ps(55) = 1;         M_.px(55) = 1;      
%   M_.p0(56) = -0.017384623112;      M_.ps(56) = 1;         M_.px(56) = 1;      
%   M_.p0(57) = -0.017627999796;      M_.ps(57) = 1;         M_.px(57) = 1; 


%__________________________________________________________________________
%  STACKED UNIVARIATE
%  WITH RANDOM WALK 
%  CTR AND HOUSE PRICE SLOPE RESTRICTED
%  NO CAR
%  Likhood  = 2199.73037330
%__________________________________________________________________________
%   M_.p0     =  zeros(1,60);
%   M_.px     =  zeros(1,60);
%   M_.ps     =   ones(1,60);
%   M_.p0(1)  =  0.218522059528;      M_.ps(1)  = 1;         M_.px(1)  = 1;      
%   M_.p0(2)  =  0.079268936962;      M_.ps(2)  = 1;         M_.px(2)  = 1;      
%   M_.p0(3)  =  0.071179908012;      M_.ps(3)  = 1;         M_.px(3)  = 1;      
%   M_.p0(4)  = -0.196222478418;      M_.ps(4)  = 1;         M_.px(4)  = 1;      
%   M_.p0(5)  =  0.199874141220;      M_.ps(5)  = 1;         M_.px(5)  = 1;      
%   M_.p0(6)  =  0.147151575523;      M_.ps(6)  = 1;         M_.px(6)  = 1;      
%   M_.p0(10) = -0.000000813242;      M_.ps(10) = 100;       M_.px(10) = 1;      
%   M_.p0(11) = -0.000000036030;      M_.ps(11) = 100;       M_.px(11) = 1;      
%   M_.p0(12) = -0.000000565688;      M_.ps(12) = 100;       M_.px(12) = 1;      
%   M_.p0(20) =  0.000531348584;      M_.ps(20) = 100;       M_.px(20) = 1;      
%   M_.p0(21) =  0.001000000000;      M_.ps(21) = 1000;      M_.px(21) = 0;      
%   M_.p0(22) =  0.001000000000;      M_.ps(22) = 1000;      M_.px(22) = 0;      
%   M_.p0(30) =  0.006483052589;      M_.ps(30) = 100;       M_.px(30) = 1;      
%   M_.p0(34) =  0.003314584739;      M_.ps(34) = 100;       M_.px(34) = 1;      
%   M_.p0(38) =  0.008897673049;      M_.ps(38) = 100;       M_.px(38) = 1;      
%   M_.p0(55) =  0.008407283108;      M_.ps(55) = 1;         M_.px(55) = 1;      
%   M_.p0(56) =  0.002511727608;      M_.ps(56) = 1;         M_.px(56) = 1;      
%   M_.p0(57) = -0.018715058186;      M_.ps(57) = 1;         M_.px(57) = 1;

   
%__________________________________________________________________________
%  STACKED UNIVARIATE
%  WITH RANDOM WALK
%  CTR AND HOUSE PRICE SLOPE RESTRICTED
%  WITH CAR
%  Likhood  = 2267.41702181
%
%  LR TEST against all CAR = 0
%  LR CAR = 2*(2267.41-2199.730)= 135.36
%  df = 3, p = 0
%
%  LR test against GF3_S123
%  LR = 2*(2267.417-2262.267) =  10.300
%  df = 6, p =  .112
%
%  LR test against GF3_S23
%  LR = 2*(2267.417-2265.120) =  4.5940
%  df = 3, p = .204
%__________________________________________________________________________
  M_.p0     =  zeros(1,60);
  M_.px     =  zeros(1,60);
  M_.ps     =   ones(1,60);
  M_.p0(1)  =  0.258808638355;      M_.ps(1)  = 1;         M_.px(1)  = 1;      
  M_.p0(2)  =  0.199941006760;      M_.ps(2)  = 1;         M_.px(2)  = 1;      
  M_.p0(3)  =  0.208337318294;      M_.ps(3)  = 1;         M_.px(3)  = 1;      
  M_.p0(4)  = -0.175116920591;      M_.ps(4)  = 1;         M_.px(4)  = 1;      
  M_.p0(5)  =  0.163991711097;      M_.ps(5)  = 1;         M_.px(5)  = 1;      
  M_.p0(6)  =  0.103998582440;      M_.ps(6)  = 1;         M_.px(6)  = 1;      
  M_.p0(7)  =  1.043672368224;      M_.ps(7)  = 1;         M_.px(7)  = 1;      
  M_.p0(8)  = 11.236046135426;      M_.ps(8)  = 1;         M_.px(8)  = 1;      
  M_.p0(9)  =  2.387979587732;      M_.ps(9)  = 1;         M_.px(9)  = 1;      
  M_.p0(10) =  0.005317964094;      M_.ps(10) = 100;       M_.px(10) = 1;      
  M_.p0(11) =  0.002623689078;      M_.ps(11) = 100;       M_.px(11) = 1;      
  M_.p0(12) =  0.000082752924;      M_.ps(12) = 100;       M_.px(12) = 1;      
  M_.p0(20) =  0.000369084719;      M_.ps(20) = 100;       M_.px(20) = 1;      
  M_.p0(21) =  0.001000000000;      M_.ps(21) = 1000;      M_.px(21) = 0;      
  M_.p0(22) =  0.001000000000;      M_.ps(22) = 1000;      M_.px(22) = 0;      
  M_.p0(30) =  0.004148938151;      M_.ps(30) = 100;       M_.px(30) = 1;      
  M_.p0(34) =  0.002137556491;      M_.ps(34) = 100;       M_.px(34) = 1;      
  M_.p0(38) =  0.007549563088;      M_.ps(38) = 100;       M_.px(38) = 1;      
  M_.p0(55) =  0.014423315085;      M_.ps(55) = 1;         M_.px(55) = 1;      
  M_.p0(56) = -0.017375553916;      M_.ps(56) = 1;         M_.px(56) = 1;      
  M_.p0(57) = -0.017516238758;      M_.ps(57) = 1;         M_.px(57) = 1; 
  
  
%__________________________________________________________________________
% Estimation options
%__________________________________________________________________________
  opt_flag   = 0;                                      
  smo_flag   = 1;                                   
  tst_flag   = 1;
  
  options = optimset('fminunc');
  options = optimset(options,'Display'       ,'iter');  
  options = optimset(options,'FunValCheck'   ,'on');
  options = optimset(options,'TolFun'        ,1e-5);
  options = optimset(options,'TolX'          ,1e-5);
  options = optimset(options,'MaxFunEvals'   ,20000);
  options = optimset(options,'MaxIter'       ,100);

%__________________________________________________________________________
% Parameter restrictions
% Set M_.px(i)= 0 to exclude M_.p(i) from estimation, M_.px(i)= 1 otherwise
%__________________________________________________________________________
  M_.px(1:3)   = [1 1 1];                 % Rho    1-3
  M_.px(4:6)   = [1 1 1];                 % Lambda 1-3
  M_.px(7:9)   = [1 1 1];                 % CAR1   1-3

  M_.px(50:52) = [0 0 0];                 % Irregular   
  
  M_.px(10:12) = [1 1 1];                 % RW  
  M_.px(14:16) = [0 0 0];
  
  M_.px(20:22) = [1 0 0];                 % Slope 
  M_.px(24:26) = [0 0 0];
  
  M_.px(30:38) = [1 0 0;                  % A
                  0 1 0; 
                  0 0 1];                 

  M_.px(40:45) = [0 0 0 0 0 0];           % A* 
  
  M_.px(46:48) = [0 0 0];                 % corr Sig_kappa 
  M_.px(55:59) = [1 1 1 0 0];             % Dummies 
  
  
%__________________________________________________________________________
%  Estimation                                                    
%__________________________________________________________________________
  p_  = select(M_.p0,M_);
   if opt_flag 
     [p_,lv,flag,o_,g_,h_] = fminunc('DKFLoglik',p_,options,Ym,M_);
      cov_  = inv(h_);
   else
      lv    = DKFLoglik(p_,Ym,M_);
      cov_  = nan;
      flag  = 9;
   end
   p_  = deselect(p_,M_.p0,M_);
   eval(['S = ' M_.Model '(p_,[],0);'])
   R   = DKF(Ym,M_.Model,p_);
   opt_outp(flag,lv,p_,cov_,M_,size(Ym,1)); 
   
%__________________________________________________________________________
% Smoothing                                                  
%__________________________________________________________________________
  if smo_flag
     R_s    = AKF(Ym ,M_.Model,p_); 
     R_s    = ADS(R_s,M_.Model,p_);
     R_s    = STS(R_s,M_.Model,p_);
     [Trends,Slopes,Cycles] = graphs_mv(Date,Ym,S.Z,R,R_s,M_,Label); 
  end  
  
%__________________________________________________________________________
% Diagnostics
%__________________________________________________________________________
  if tst_flag
      Sv_      = UOC_tests(Date,Ym,R,S.nd);        
      SG_      = GFC_Diagnostics(Ym,M_,S,p_,'',ctry);
  end
  
%__________________________________________________________________________
% Printing new starting values                                      
%__________________________________________________________________________ 
  Print_pars(p_,M_.ps,M_.px,lv,0)
  

   %Export results: data, trend, cycle
 Results=[data(13:end,:),Trends, Cycles];
 writematrix(Results,strcat(Base_Dir, '\Outcome\Results_', name_results, '.xlsx'));
 
 name_results="oneside_univar";  
 ONESIDE= Cycles_t.C20;
 writematrix(ONESIDE,strcat(Base_Dir, '\Outcome\Results_', name_results, '.xlsx'));
 