%__________________________________________________________________________
% MAIN PROGRAM FOR THE DIFFUSE TIME VARIANT KALMAN-FILTER         
% Estimates the model for US GDP/CTR/HPR with slopes for CTR and HPR 
% restricted to .001. Similar cycles for CTR and HPR imposed.  
% Uncomment Pseudo Real Time only if needed, it might be computationally
% intensive
%__________________________________________________________________________

clear
clc
Base_Dir = pwd;
addpath(genpath(Base_Dir));
 
%__________________________________________________________________________
% Data - Main model 
%__________________________________________________________________________
name_results="main";  
ctry  = 'IE';

T = readtable(fullfile(Base_Dir,'data_model.csv'), ...
              'ReadVariableNames', false, 'Delimiter', ',');

DateStr = string(T{:,1});
X       = T{:,2:4};       

y = double(extractBefore(DateStr,'q'));
q = double(extractAfter (DateStr,'q'));
Date = [datenum(datetime(y, q*3, 1))];  % [datenum] 

data = X;
Ym       = X(13:end,1:3);

Date     = Date(13:end,1);
Label{1} = [ctry,': GDP'];
Label{2} = [ctry,': CTR'];
Label{3} = [ctry,': HPR'];

%__________________________________________________________________________
% Data - Pseudo Real Time 
%__________________________________________________________________________
% global data_limit name_results;
% 
% T = readtable(fullfile(Base_Dir,'data_model.csv'), ...
%               'ReadVariableNames', false, 'Delimiter', ',');
% 
% DateStr = string(T{:,1});
% X       = T{:,2:4};       
% 
% y = double(extractBefore(DateStr,'q'));
% q = double(extractAfter (DateStr,'q'));
% Date = [datenum(datetime(y, q*3, 1))];  % [datenum] 
% 
% X = data(1:data_limit, :);
% data = data(1:data_limit, :);
% Date = Date(1:data_limit, 1);
% 
% Ym   = data(13:end, 1:3);
% Date = Date(13:end, 1);
% 
%__________________________________________________________________________
% Model
% p(40) - p(42) contain the phase shifts xi(2) ... xi(n)
% p(43) - p(45) contain the elements of X where D =  (I-X)^(1) (I+X).
%               and X is skew-symmetric
% Given that the 1st row/col of D is unity, the same applies to X
%__________________________________________________________________________
  M_.Model   =  'GF3_S23_US_DYN';
  
  M_.Trend   =  1:3;
  M_.Slope   =  4:6;
  M_.Cycle   =  13:18;
   
% Parameter names 
% Cyc dynamics
  M_.p_n{1}  = 'rho1' ; M_.p_n{2}  = 'rho2';    
  M_.p_n{4}  = 'lam1' ; M_.p_n{5}  = 'lam2'; 
  M_.p_n{7}  = 'car1' ; M_.p_n{8}  = 'car2'; 
    
% Trends and slope  
  M_.p_n{10} = 'RW1 ' ; M_.p_n{11} = 'RW2 ' ; M_.p_n{12} = 'RW3' ; 
  M_.p_n{14} = 'RW21' ; M_.p_n{15} = 'RW31' ; M_.p_n{16} = 'RW32'; 
 
  M_.p_n{20} = 'SL1 ' ; M_.p_n{21} = 'SL2 ' ; M_.p_n{22} = 'SL3 '; 
  M_.p_n{24} = 'SL21' ; M_.p_n{25} = 'SL31' ; M_.p_n{26} = 'SL32'; 
 
% Factor loadings  
  M_.p_n{30} = 'A11 ' ; M_.p_n{31} = 'A12 ' ; 
  M_.p_n{33} = 'A21 ' ; M_.p_n{34} = 'A22 ' ; M_.p_n{35} = 'A23 '; 
  M_.p_n{36} = 'A31 ' ; M_.p_n{37} = 'A32 ' ; M_.p_n{38} = 'A33 ';

  M_.p_n{40} = 'A*21' ; M_.p_n{41} = 'A*22' ;
  M_.p_n{43} = 'A*31' ; M_.p_n{44} = 'A*32' ; M_.p_n{45} = 'A*33'; 
  
% Corr cycles  
  M_.p_n{46} = 'C12 ' ; M_.p_n{47} = 'C21 ' ;

% Irreg  
  M_.p_n{50} = 'eps1' ; M_.p_n{51} = 'eps2' ; M_.p_n{52} = 'eps3' ;
  
  M_.p_n{55} = 'dum1' ; M_.p_n{56} = 'dum2' ; M_.p_n{57} = 'dum3';
  M_.p_n{58} = 'dum4' ; M_.p_n{59} = 'dum5' ; M_.p_n{60} = 'dum6';

  
%__________________________________________________________________________
% TEMPLATE starting values general model 
%__________________________________________________________________________
  M_.p0     =  zeros(1,60);
  M_.ps     =   ones(1,60);
  
% Cycle dynamics  
  M_.p0(1)  =  0.142000000000;      M_.ps(1)  = 1;         M_.px(1)  = 0;      
  M_.p0(2)  =  0.142000000000;      M_.ps(2)  = 1;         M_.px(2)  = 0;      
   
  M_.p0(4)  =  0.112958531714;      M_.ps(4)  = 1;         M_.px(4)  = 0;      
  M_.p0(5)  =  0.112958531714;      M_.ps(5)  = 1;         M_.px(5)  = 0;      
  
  M_.p0(7)  =  0.000000000000;      M_.ps(7)  = 1;         M_.px(7)  = 0;      
  M_.p0(8)  =  0.000000000000;      M_.ps(8)  = 1;         M_.px(8)  = 0;      
  
% Level 
  M_.p0(10) =  0.001225080626;      M_.ps(10) = 1000;      M_.px(10) = 1;      
  M_.p0(11) =  0.000136956720;      M_.ps(11) = 1000;      M_.px(11) = 1;      
  M_.p0(12) =  0.000056829278;      M_.ps(12) = 1000;      M_.px(12) = 1;      
  M_.p0(14) =  0.000000000000;      M_.ps(14) = 10;        M_.px(14) = 0;      
  M_.p0(15) =  0.000000000000;      M_.ps(15) = 10;        M_.px(15) = 0;      
  M_.p0(16) =  0.000000000000;      M_.ps(16) = 10;        M_.px(16) = 0;      
  
% Slope  
  M_.p0(20) =  0.000500000000;      M_.ps(20) = 1000;      M_.px(20) = 0;      
  M_.p0(21) =  0.000800000000;      M_.ps(21) = 1000;      M_.px(21) = 0;      
  M_.p0(22) =  0.000800000000;      M_.ps(22) = 1000;      M_.px(22) = 0;      
  
% Loadings A  
  M_.p0(30) =  0.005391199926;      M_.ps(30) = 100;       M_.px(30) = 1;      
  M_.p0(31) =  0.000000000000;      M_.ps(31) = 100;       M_.px(31) = 0;      
        
  M_.p0(33) =  0.000000000000;      M_.ps(33) = 100;       M_.px(33) = 0;      
  M_.p0(34) =  0.074948979361;      M_.ps(34) = 100;       M_.px(34) = 1;      
  M_.p0(35) =  0.000000000000;      M_.ps(35) = 100;       M_.px(35) = 0;      
  M_.p0(36) =  0.000000000000;      M_.ps(36) = 100;       M_.px(36) = 0;      
  M_.p0(37) =  0.000000000000;      M_.ps(37) = 100;       M_.px(37) = 0;      
  M_.p0(38) =  0.082413809184;      M_.ps(38) = 100;       M_.px(38) = 1;      
  
% Loadings A_s  
  M_.p0(40) =  0.000000000000;      M_.ps(41) = 100;       M_.px(41) = 0;      
  M_.p0(41) =  0.000000000000;      M_.ps(41) = 100;       M_.px(41) = 0;      
       
  M_.p0(43) =  0.000000000000;      M_.ps(41) = 100;       M_.px(41) = 0;      
  M_.p0(44) =  0.000000000000;      M_.ps(41) = 100;       M_.px(41) = 0;      
  M_.p0(45) =  0.000000000000;      M_.ps(41) = 100;       M_.px(41) = 0;      
  
% Corr Sig_kappa  
  M_.p0(46) =  0.500000000000;      M_.ps(46) = 1;         M_.px(46) = 0;      
  M_.p0(47) =  0.500000000000;      M_.ps(47) = 1;         M_.px(47) = 0;      
  
% Irreg  
  M_.p0(50) =  0.000001000000;      M_.ps(50) = 1000;      M_.px(50) = 0;      
  M_.p0(51) =  0.000001000000;      M_.ps(51) = 1000;      M_.px(51) = 0;      
  M_.p0(52) =  0.000001000000;      M_.ps(52) = 1000;      M_.px(52) = 0; 

%__________________________________________________________________________
%  FINAL ESTIMATE FROM RUNSTLER / VLEKKE
%  RUN THIS WITH M_.MODEL = 'GF3_S23_US'
%  Likhood  = 2291.79541373
% These initial values are the same as the ones used in the original code
% published in JOA in GF2_S23_US_FIX.main - Farah
%__________________________________________________________________________
  M_.p0     =  zeros(1,60);
  M_.px     =  zeros(1,60);
  M_.ps     =   ones(1,60);
  M_.p0(1)  =  0.389048712723;      M_.ps(1)  = 1;         M_.px(1)  = 1;      
  M_.p0(2)  =  0.211061342133;      M_.ps(2)  = 1;         M_.px(2)  = 1;      
  M_.p0(4)  =  0.329311258640;      M_.ps(4)  = 1;         M_.px(4)  = 1;      
  M_.p0(5)  =  0.145508595851;      M_.ps(5)  = 1;         M_.px(5)  = 1;      
  M_.p0(8)  =  2.579828643143;      M_.ps(8)  = 1;         M_.px(8)  = 1;      
  M_.p0(20) =  0.000307792471;      M_.ps(20) = 100;       M_.px(20) = 1;      
  M_.p0(21) =  0.001000000000;      M_.ps(21) = 1000;      M_.px(21) = 0;      
  M_.p0(22) =  0.001000000000;      M_.ps(22) = 1000;      M_.px(22) = 0;      
  M_.p0(24) =  0.567676542100;      M_.ps(24) = 1;         M_.px(24) = 1;      
  M_.p0(25) =  45.406328822911;     M_.ps(25) = 1;         M_.px(25) = 1;      
  M_.p0(26) =  112.907455518106;    M_.ps(26) = 1;         M_.px(26) = 1;      
  M_.p0(30) =  0.005633910252;      M_.ps(30) = 100;       M_.px(30) = 1;      
  M_.p0(31) =  0.001326793387;      M_.ps(31) = 100;       M_.px(31) = 1;      
  M_.p0(33) =  0.000089428239;      M_.ps(33) = 100;       M_.px(33) = 1;      
  M_.p0(34) =  0.001549884812;      M_.ps(34) = 100;       M_.px(34) = 1;      
  M_.p0(35) = -0.000962005566;      M_.ps(35) = 100;       M_.px(35) = 1;      
  M_.p0(36) = -0.001052684907;      M_.ps(36) = 100;       M_.px(36) = 1;      
  M_.p0(37) =  0.005331411446;      M_.ps(37) = 100;       M_.px(37) = 1;      
  M_.p0(38) =  0.004033520713;      M_.ps(38) = 100;       M_.px(38) = 1;      
  M_.p0(40) = -0.002340646118;      M_.ps(40) = 100;       M_.px(40) = 1;      
  M_.p0(41) = -0.001332709279;      M_.ps(41) = 100;       M_.px(41) = 1;      
  M_.p0(43) =  0.001180688669;      M_.ps(43) = 100;       M_.px(43) = 1;      
  M_.p0(44) = -0.001728221555;      M_.ps(44) = 100;       M_.px(44) = 1;      
  M_.p0(45) = -0.001176193067;      M_.ps(45) = 100;       M_.px(45) = 1;      
  M_.p0(55) =  0.014362464640;      M_.ps(55) = 1;         M_.px(55) = 1;      
  M_.p0(56) = -0.017467442261;      M_.ps(56) = 1;         M_.px(56) = 1;      
  M_.p0(57) = -0.015893654614;      M_.ps(57) = 1;         M_.px(57) = 1; 

  
%__________________________________________________________________________
%  ESTIMATE FROM DYN
%  RUN THIS WITH M_.MODEL = 'GF3_S23_US_DYN'
%  Likhood  
% %__________________________________________________________________________
  M_.p0     =  zeros(1,60);
  M_.px     =  zeros(1,60);
  M_.ps     =   ones(1,60);
  M_.p0(1)  =  0.8337;              M_.ps(1)  = 1;         M_.px(1)  = 1;      
  M_.p0(2)  =  0.9133;              M_.ps(2)  = 1;         M_.px(2)  = 1;      
  M_.p0(4)  =  0.5818;              M_.ps(4)  = 1;         M_.px(4)  = 1;      
  M_.p0(5)  =  0.1402;              M_.ps(5)  = 1;         M_.px(5)  = 1;      
  M_.p0(7)  =  0.6108;              M_.ps(7)  = 1;         M_.px(7)  = 1;      
  M_.p0(8)  =  0.9097;              M_.ps(8)  = 1;         M_.px(8)  = 1;      
  
% Level  
  M_.p0(10) =  0.0013;              M_.ps(10) = 100;       M_.px(10) = 1;      
  M_.p0(11) =  0.0006;              M_.ps(11) = 1000;      M_.px(11) = 0;      
  M_.p0(12) =  0.0007;              M_.ps(12) = 1000;      M_.px(12) = 0;      
  M_.p0(13) = -0.6872;              M_.ps(13) = 1;         M_.px(13) = 1;      
  M_.p0(14) = -0.3619;              M_.ps(14) = 1;         M_.px(14) = 1;      
  M_.p0(15) =  0.2850;              M_.ps(15) = 1;         M_.px(15) = 1;      

% Slope
  M_.p0(20) =  0.0005;              M_.ps(20) = 100;       M_.px(20) = 1;      
  M_.p0(21) =  0.0010;              M_.ps(21) = 1000;      M_.px(21) = 0;      
  M_.p0(22) =  0.0007;              M_.ps(22) = 1000;      M_.px(22) = 0;      
  M_.p0(23) =  0.4950;              M_.ps(23) = 1;         M_.px(23) = 1;      
  M_.p0(24) =  0.4799;              M_.ps(24) = 1;         M_.px(24) = 1;      
  M_.p0(25) =  0.1452;              M_.ps(25) = 1;         M_.px(25) = 1;      
  
  M_.p0(30) =  0.0029;              M_.ps(30) = 100;       M_.px(30) = 1;      
  M_.p0(31) =  0.0011;              M_.ps(31) = 100;       M_.px(31) = 1;      
  M_.p0(33) =  0.0005;              M_.ps(33) = 100;       M_.px(33) = 1;      
  M_.p0(34) =  0.0024;              M_.ps(34) = 100;       M_.px(34) = 1;      
  M_.p0(35) = -0.0007;              M_.ps(35) = 100;       M_.px(35) = 1;      
  M_.p0(36) = -0.0001;              M_.ps(36) = 100;       M_.px(36) = 1;      
  M_.p0(37) =  0.0042;              M_.ps(37) = 100;       M_.px(37) = 1;      
  M_.p0(38) =  0.0047;              M_.ps(38) = 100;       M_.px(38) = 1;      
  M_.p0(40) =  0.0013;              M_.ps(40) = 100;       M_.px(40) = 1;      
  M_.p0(41) = -0.0008;              M_.ps(41) = 100;       M_.px(41) = 1;      
  M_.p0(43) = -0.0002;              M_.ps(43) = 100;       M_.px(43) = 1;      
  M_.p0(44) =  0.0022;              M_.ps(44) = 100;       M_.px(44) = 1;      
  M_.p0(45) =  0.0014;              M_.ps(45) = 100;       M_.px(45) = 1;
  
% Irregular  
  M_.p0(50) =  0.0030;              M_.ps(50) = 100;       M_.px(50) = 1;      
  M_.p0(51) =  0.0015;              M_.ps(51) = 1000;      M_.px(51) = 0;      
  M_.p0(52) =  0.0017;              M_.ps(52) = 1000;      M_.px(52) = 0;      
  
% Dummies
% I kept the ML estimates for better comparison 
  M_.p0(55) =  0.014362464640;      M_.ps(55) = 1;         M_.px(55) = 1;      
  M_.p0(56) = -0.017467442261;      M_.ps(56) = 1;         M_.px(56) = 1;      
  M_.p0(57) = -0.015893654614;      M_.ps(57) = 1;         M_.px(57) = 1; 

%__________________________________________________________________________
%  Estimation options 
%  The Similar cycles restriction implies that
%  A(1,3) = A*(2,3) = Sig_kappa(2,3) = 0
%  That is p(32) = p(42) = p(48) = 0!
%__________________________________________________________________________
% Parameter restrictions
  M_.px(1:2)   = [1 1];                   % Rho    1-2
  M_.px(4:5)   = [1 1];                   % Lambda 1-2
  M_.px(7:8)   = [0 1];                   % CAR1   1-2

  M_.px(50:52) = [0 0 0];                % Irregular   
  
  M_.px(10:12) = [0 0 0];                 % RW  
  M_.px(14:16) = [0 0 0];
  
  M_.px(20:22) = [1 0 0];                 % Slope 
  M_.px(24:26) = [1 1 1];
  
  M_.px(30:38) = [1 1 0    ...            % A  element (1,3) = 0! 
                  1 1 1    ...
                  1 1 1];                 

  M_.px(40:45) = [1 1 0 ... 
                  1 1 1];                 % A* element (1,3) = 0!
  
  M_.px(46:48) = [0 0 0];                 % corr Sig_kappa 
                                          % element (1,3) = 0!!
  M_.px(55:59) = [1 1 1 0 0];             % Dummies 

% Options
  opt_flag   = 0;                                      
  smo_flag   = 1;                                   
  tst_flag   = 1; 

  options = optimset('fminunc');
  options = optimset(options,'Display'       ,'iter');  
  options = optimset(options,'FunValCheck'   ,'on');
  options = optimset(options,'TolFun'        ,1e-6);
  options = optimset(options,'TolX'          ,1e-6);
  options = optimset(options,'MaxFunEvals'   ,20000);
  options = optimset(options,'MaxIter'       ,100);
 
% Set path for saving figures (empty to avoid saving) 
%  fpath = [Base_Dir '\FSS_V3\' ctry '\Plots\'];
  fpath = [];

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
% Diagnostics and real-time estimates
%__________________________________________________________________________
  if tst_flag
      Sv_        =  UOC_tests(Date,Ym,R,S.nd);
      SG_        =  GFC_Diagnostics(Ym,M_,S,p_,fpath,ctry); 
      [CC,Cm,Cl]  =  Sample_CCF(Cycles,SG_,20,1);
      Cycles_t   =  PRT_Estimates(Date,Ym,M_,S,p_,37,size(Ym,1)-20);
  end
 
%__________________________________________________________________________
% Christiano and Fitzgerald filter LEVELS
%__________________________________________________________________________
% Define Ym again
  Ym       = X(13:end,1:3); 
  CycCFL   = CF_filter(Ym,32,120);
  CycCFS   = CF_filter(Ym,8,32);  
  CycCF_t  = PRT_CFfilter(Date,Ym,[8 32 32],[32 120 120],37,size(Ym,1)-20); 
  
graphs_mv_annex(Date,Ym,S.Z,R,R_s,M_,fpath,ctry); 
graphs_mv_pres(Date,Ym,S.Z,R,R_s,M_,fpath,ctry); 
  
%__________________________________________________________________________
% Saving estimates of cycles to xls
%__________________________________________________________________________
%   rpath  = [Base_Dir 'FSS_V3/' ctry '/Results/' ctry];  
%     
%   save([rpath '_UOC'],'Date','Ym','Cycles','Cycles_t','R','R_s')
%   save([rpath '_CF'] ,'Date','CycCFS','CycCFL','CycCF_t')
%     
%   xlswrite([rpath '_Cycles.xls'],[Date Cycles],'UOC' ,'A2');
%   xlswrite([rpath '_Cycles.xls'],[Date CycCFL],'CF_L','A2');
%   xlswrite([rpath '_Cycles.xls'],[Date CycCFS],'CF_S','A2');
%    
%   xlswrite([rpath '_Cycles.xls'],[Date Cycles_t.C00],'C00_UC','A2');
%   xlswrite([rpath '_Cycles.xls'],[Date Cycles_t.C20],'C20_UC','A2');
%   xlswrite([rpath '_Cycles.xls'],[Date CycCF_t.C00] ,'C00_CF','A2');
%   xlswrite([rpath '_Cycles.xls'],[Date CycCF_t.C20] ,'C20_CF','A2');
    
%__________________________________________________________________________
% Printing new starting values                                      
%__________________________________________________________________________ 
 Print_pars(p_,M_.ps,M_.px,lv,0)  

 %writematrix(Cycles,[Base_Dir '\Outcome\Cycles_Dynare.xlsx'])
  
 %Export results: data, trend, cycle
 Results=[data(13:end,:),Trends, Cycles];
 writematrix(Results,strcat(Base_Dir, '\Outcome\Results_', name_results, '.xlsx'));
 
 oneside_= Cycles_t.C00;
 writematrix(oneside_,strcat(Base_Dir, '\Outcome\Results_', "oneside00", '.xlsx'));
 oneside_= Cycles_t.C20;
 writematrix(oneside_,strcat(Base_Dir, '\Outcome\Results_', "oneside20", '.xlsx'));