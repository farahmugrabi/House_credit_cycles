%
% ==== ECB WGEM Real and Financial Cycles Team 2016 ====
%

% ---------- MULTIVARIATE UNOBSERVED COMPONENTS MODEL FOR CYCLES EXTRACTION ----------
% This Dynare code is a re-implementation of GF3_S123.m in BF Cycles code.zip Refer to
% Rünstler and Vlekke (2015) "Business and Financial Cycles: an Unobserved Components 
% Model Perspecvive", mimeo
%
% (c) May 2016 by Dmitry.Kulikov@eestipank.ee

% =======> B E F O R E  Y O U  S T A R T ,  R E A D  T H I S  C A R E F U L L Y <=======
%
% If you use Dynare 4.4.3 stable version, you will need to manually edit a file in Dynare
% folder on your harddrive named dynare/4.4.3/matlab/missing_DiffuseKalmanSmootherH1_Z.m 
% and change two lines in it according to the following instructions:
% https://github.com/DynareTeam/dynare/commit/42ecfa382f555a2d9eaeec792223844ad8c3d9ab
%
% ================================> T H A N K  Y O U ! <================================
 

%----------------------------------------------------------------
% 1. Model variables and parameters
%----------------------------------------------------------------

var		bbeta1		$\beta_{1t}$,
		bbeta2		$\beta_{2t}$,
		bbeta3		$\beta_{3t}$,
		mmu1		$\mu_{1t}$,
		mmu2		$\mu_{2t}$,
		mmu3		$\mu_{3t}$,
		ppsi1		$\psi_{1t}$,
		ppsi2		$\psi_{2t}$,
		ppsi3		$\psi_{3t}$,
		ppsist1		$\psi^{*}_{1t}$,
		ppsist2		$\psi^{*}_{2t}$,
		ppsist3		$\psi^{*}_{3t}$,
		car1,
		car2,
		car3,
		carst1,
		carst2,
		carst3,
		ym1		$y_{1t}$,
		ym2		$y_{2t}$,
		ym3		$y_{3t}$;

varexo		e_irr1		$\epsilon_{1t}$,
		e_irr2		$\epsilon_{2t}$,
		e_irr3		$\epsilon_{3t}$,
		e_lev1		$\eta_{1t}$,
		e_lev2		$\eta_{2t}$,
		e_lev3		$\eta_{3t}$,
		e_slp1		$\zeta_{1t}$,
		e_slp2		$\zeta_{2t}$,
		e_slp3		$\zeta_{3t}$,
		e_cyc1		$\kappa_{1t}$,
		e_cyc2		$\kappa_{2t}$,
		e_cyc3		$\kappa_{3t}$,
		e_cycst1	$\kappa^{*}_{1t}$,
		e_cycst2	$\kappa^{*}_{2t}$,
		e_cycst3	$\kappa^{*}_{3t}$;

parameters	rrho		$\rho$,
		pphi		$\phi$,
		llambda		$\lambda$,
		a11,
		a21, a22,
		a31, a32, a33,
		ast21,
		ast31, ast32;


%----------------------------------------------------------------
% 2. Calibration
%----------------------------------------------------------------

rrho		= 0.7500;
pphi		= 0.7500;
llambda		= 0.2000;
a21		= 0.0000;
a31		= 0.0000;
a32		= 0.0000;
ast21		= 0.0000;
ast31		= 0.0000;
ast32		= 0.0000;


%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------

model(linear);


%%%%% STOCHASTIC TRENDS AND SLOPES %%%%%

mmu1 		=    mmu1(-1)	 + bbeta1(-1)	   + e_lev1;
bbeta1		=		   bbeta1(-1)	   + e_slp1;

mmu2 		=    mmu2(-1)	 + bbeta2(-1)	   + e_lev2;
bbeta2		=		   bbeta2(-1)	   + e_slp2;

mmu3 		=    mmu3(-1)	 + bbeta3(-1)	   + e_lev3;
bbeta3		=		   bbeta3(-1)	   + e_slp3;


%%%%% CYCLICAL AUTOREGRESSIONS %%%%%

#rc		= rrho*cos(llambda);
#rs		= rrho*sin(llambda);

ppsi1		= rc*ppsi1(-1) 	 + rs*ppsist1(-1)  + e_cyc1;
ppsist1		=-rs*ppsi1(-1) 	 + rc*ppsist1(-1)  + e_cycst1;
car1		=    ppsi1(-1) 	 + pphi*car1(-1)   + 0;
carst1		=    ppsist1(-1) + pphi*carst1(-1) + 0;

ppsi2		= rc*ppsi2(-1)	 + rs*ppsist2(-1)  + e_cyc2;
ppsist2		=-rs*ppsi2(-1)	 + rc*ppsist2(-1)  + e_cycst2;
car2		=    ppsi2(-1)	 + pphi*car2(-1)   + 0;
carst2		=    ppsist2(-1) + pphi*carst2(-1) + 0;

ppsi3		= rc*ppsi3(-1)	 + rs*ppsist3(-1)  + e_cyc3;
ppsist3		=-rs*ppsi3(-1)	 + rc*ppsist3(-1)  + e_cycst3;
car3		=    ppsi3(-1)	 + pphi*car3(-1)   + 0;
carst3		=    ppsist3(-1) + pphi*carst3(-1) + 0;


%%%%% MEASUREMENT EQUATIONS %%%%%

ym1		=    mmu1(-1)	 + ( a11*car1(-1) +     0	 +     0	) + (       0	       +       0	  + 0 )	+ e_irr1;
ym2		=    mmu2(-1)	 + ( a21*car1(-1) + a22*car2(-1) +     0	) + ( ast21*carst1(-1) +       0	  + 0 )	+ e_irr2;
ym3		=    mmu3(-1)	 + ( a31*car1(-1) + a32*car2(-1) + a33*car3(-1)	) + ( ast31*carst1(-1) + ast32*carst2(-1) + 0 )	+ e_irr3;

end;

shocks;

var e_cyc1; 	stderr 1.0000;
var e_cycst1;	stderr 1.0000;

var e_cyc2; 	stderr 1.0000;
var e_cycst2;	stderr 1.0000;

var e_cyc3; 	stderr 1.0000;
var e_cycst3;	stderr 1.0000;

end;


%----------------------------------------------------------------
% 4. Estimated parameters and data
%----------------------------------------------------------------

estimated_params;

%%%%% PARAMETERS %%%%%

	rrho,		beta_pdf,       0.75, 0.20;
	pphi,		beta_pdf,       0.75, 0.20;
	llambda,	normal_pdf,     0.20, 0.20;
	a11,		inv_gamma_pdf,  0.005, inf;
	a22,		inv_gamma_pdf,  0.005, inf;
	a33,		inv_gamma_pdf,  0.005, inf;

%%%%% INNOVATIONS %%%%%

stderr	e_irr1,		inv_gamma_pdf,  0.005, inf;
stderr	e_irr2,		inv_gamma_pdf,  0.005, inf;
stderr	e_irr3,		inv_gamma_pdf,  0.005, inf;

stderr	e_lev1,		inv_gamma_pdf,  0.001, inf;
stderr	e_lev2,		inv_gamma_pdf,  0.001, inf;
stderr	e_lev3,		inv_gamma_pdf,  0.001, inf;
corr	e_lev1, e_lev2,	beta_pdf, 	0.00, 0.30, -1, 1;
corr	e_lev1, e_lev3,	beta_pdf, 	0.00, 0.30, -1, 1;
corr	e_lev2, e_lev3,	beta_pdf, 	0.00, 0.30, -1, 1;

stderr	e_slp1,		inv_gamma_pdf,  0.001, inf;
stderr	e_slp2,		inv_gamma_pdf,  0.001, inf;
stderr	e_slp3,		inv_gamma_pdf,  0.001, inf;
corr	e_slp1, e_slp2,	beta_pdf, 	0.00, 0.30, -1, 1;
corr	e_slp1, e_slp3,	beta_pdf, 	0.00, 0.30, -1, 1;
corr	e_slp2, e_slp3,	beta_pdf, 	0.00, 0.30, -1, 1;

end;

%
% Observed variables
%

varobs ym1, ym2, ym3;	% See US_data.m for the correspondence between these
			% observables and US GDP, Credit and Prices series


%----------------------------------------------------------------
% 5. Bayesian estimation
%----------------------------------------------------------------

estimation	(
		datafile = US_data,
		nodisplay,
		graph_format = pdf,
		nodiagnostic,
		diffuse_filter,
		kalman_algo = 0,
		mh_replic = 100,
		mh_nblocks = 1,
		filtered_vars,
		smoother,
		mode_compute = 6,
		plot_priors = 0,
		forecast = 0
		) mmu1 mmu2 mmu3 car1 car2 car3;

%----------------------------------------------------------------
% 6. Reporting
%----------------------------------------------------------------

verbatim;

US_data;
stat_		= 'Mean';
label_		= { 'US log real GDP', 'US log real Credit', 'US log real House Prices' };

for i = 1:3

	eval([ 'ym_	= ym' 					 num2str(i) ';' ]);
	eval([ 'trend_ 	= oo_.SmoothedVariables.' stat_ '.mmu'   num2str(i) ';' ]);
	eval([ 'cycle_ 	= oo_.SmoothedVariables.' stat_ '.car'   num2str(i) ';' ]);
	eval([ 'irreg_ 	= oo_.SmoothedShocks.'    stat_ '.e_irr' num2str(i) ';' ]);

	graphs_UOC( Date, ym_, trend_, cycle_, irreg_, [ 'Series: ' label_{i} ] );

end;

end;





