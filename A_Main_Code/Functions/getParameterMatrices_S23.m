function [ ZZ_, TT_, HH_ ] = getParameterMatrices_S23(oo_, varargin)

%
% ==== ECB WGEM Real and Financial Cycles Team 2016 ====
%

% Get Z, T and H state-space matrices of parameters as in Gerhard's GF3_S23_US.m code
%
% (c) September 2016 by Dmitry.Kulikov@eestipank.ee



%--------------------------------------------------------------------------
% 1. Select mean or mode as the point estimate for the state-space matrices
%--------------------------------------------------------------------------

if length(varargin) == 0
	stat_	= lower('Mean');		% Default parameter
else
	stat_	= lower(varargin{1});		% Supplied parameter
end

eval([ 'myparams_ = oo_.posterior_' stat_ '.parameters; ' ]);
eval([ 'mysksstd_ = oo_.posterior_' stat_ '.shocks_std; ' ]);
eval([ 'myskscor_ = oo_.posterior_' stat_ '.shocks_corr;' ]);


%--------------------------------------------------------------------------
% 2. Lifted from Gerhard's GF3_S23_US.m code
%--------------------------------------------------------------------------

n    		= 3;				    % Number of observables in the model
ZZ_  		= zeros(  n,6*n);		% Base matrices
TT_  		= zeros(6*n,6*n);
HH_  		= zeros(6*n,5*n);

xP_   		= 2*n+1:4*n;			% Indices cyclical components and shocks
xC_   		= 4*n+1:6*n;
xS_   		= 2*n+1:6*n;
xk_   		= 3*n+1:5*n;



%--------------------------------------------------------------------------
% 3. SC and CAR parameters
%--------------------------------------------------------------------------

for i = 1:2
	if isfield(myparams_, [ 'llambda' num2str(i) ])
		eval([ 'llambda' num2str(i) '_ = myparams_.llambda' num2str(i) ';' ]);
	else
		eval([ 'llambda' num2str(i) '_ = 0.00;' ]);
	end

	if isfield(myparams_, [ 'rrho' num2str(i) ])
		eval([ '   rrho' num2str(i) '_ = myparams_.rrho'    num2str(i) ';' ]);
	else
		eval([ '   rrho' num2str(i) '_ = 0.00;' ]);
	end

	if isfield(myparams_, [ 'pphi' num2str(i) ])
		eval([ '   pphi' num2str(i) '_ = myparams_.pphi'    num2str(i) ';' ]);
	else
		eval([ '   pphi' num2str(i) '_ = 0.00;' ]);
	end
end

RC		= zeros(n, 1);			% SC parameters
RS		= zeros(n, 1);
RC(1)		= rrho1_*cos(llambda1_);
RS(1)		= rrho1_*sin(llambda1_);
RC(2)		= rrho2_*cos(llambda2_);
RS(2)		= rrho2_*sin(llambda2_);
RC(3)		= RC(2);			% Restrictions on SC parameters
RS(3)		= RS(2);
TC_		= [ diag(RC) diag(RS); diag(-RS) diag(RC) ];

CAR		= zeros(n, 1);			% CAR parameters
CAR(1)		= pphi1_;
CAR(2)		= pphi2_;
CAR(3)		= CAR(2);			% Restrictions on CAR parameters
CAR1_		= diag(CAR);



%--------------------------------------------------------------------------
% 4. Loadings in A and A* matrices
%--------------------------------------------------------------------------

for r = 1:n
for c = 1:n
	if isfield(myparams_, [ 'a'   num2str(r) num2str(c) ])
		eval([ 'aa' num2str(r) num2str(c) '_   = myparams_.a'   num2str(r) num2str(c) ';' ]);
	else
		eval([ 'aa' num2str(r) num2str(c) '_   = 0.00;' ]);
	end
end
end

for r = 1:n
for c = 1:n
	if isfield(myparams_, [ 'ast' num2str(r) num2str(c) ])
		eval([ 'aast' num2str(r) num2str(c) '_ = myparams_.ast' num2str(r) num2str(c) ';' ]);
	else
		eval([ 'aast' num2str(r) num2str(c) '_ = 0.00;' ]);
	end
end
end

AA_		= [ aa11_   aa12_   aa13_  ; aa21_   aa22_   aa23_  ; aa31_   aa32_   aa33_   ];
AAst    = [ aast11_ aast12_ aast13_; aast21_ aast22_ aast23_; aast31_ aast32_ aast33_ ];


%--------------------------------------------------------------------------
% 5. Variance-covariance matrix of the cyclical innovations
%--------------------------------------------------------------------------

CC_		= ld_mat(eye(n),zeros(n, 1));	% Identity matrix by default


%--------------------------------------------------------------------------
% 6. Trends and slopes
%--------------------------------------------------------------------------

for i = 1:n
	if isfield(mysksstd_, [ 'e_lev' num2str(i) ])
		eval([ 'lev' num2str(i) '_ = mysksstd_.e_lev' num2str(i) ';' ]);
	else
		eval([ 'lev' num2str(i) '_ = 0.00;' ]);
	end
end

for r = 1:n-1
for c = r+1:n
	if isfield(myskscor_, [ 'e_lev' num2str(r) '_e_lev' num2str(c) ])
		eval([ 'lev' num2str(r) num2str(c) '_ = myskscor_.e_lev' num2str(r) '_e_lev' num2str(c) ';' ]);
	else
		eval([ 'lev' num2str(r) num2str(c) '_ = 0.00;' ]);
	end
end
end

for i = 1:n
	if isfield(mysksstd_, [ 'e_slp' num2str(i) ])
		eval([ 'slp' num2str(i) '_ = mysksstd_.e_slp' num2str(i) ';' ]);
	else
		eval([ 'slp' num2str(i) '_ = 0.00;' ]);
	end
end

for r = 1:n-1
for c = r+1:n
	if isfield(myskscor_, [ 'e_slp' num2str(r) '_e_slp' num2str(c) ])
		eval([ 'slp' num2str(r) num2str(c) '_ = myskscor_.e_slp' num2str(r) '_e_slp' num2str(c) ';' ]);
	else
		eval([ 'slp' num2str(r) num2str(c) '_ = 0.00;' ]);
	end
end
end

HHlev_		= ld_mat(diag([ lev1_ lev2_ lev3_ ]), [ lev12_ lev13_ lev23_ ]);
HHslp_		= ld_mat(diag([ slp1_ slp2_ slp3_ ]), [ slp12_ slp13_ slp23_ ]);


%--------------------------------------------------------------------------
% 7. State-space matrices
%--------------------------------------------------------------------------

TT_(1:2*n,1:2*n)= kron([1 1; 0 1], eye(n));
TT_(xP_,xP_)	= TC_; 
TT_(xC_,xP_)	= eye(6);
TT_(xC_,xC_)	= kron(eye(2), CAR1_);

HH_(1:n,n+1:2*n)= HHlev_;
HH_(n+1:2*n,2*n+1:3*n) = HHslp_;
HH_(xP_,xk_)	= kron(eye(2), CC_);

ZZ_(1:n,1:n)	= eye(n);
ZZ_(1:n,4*n+1:5*n) =  AA_;
ZZ_(1:n,5*n+1:6*n) =  AAst_;


% keyboard;



