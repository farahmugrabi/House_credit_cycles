%
% ==== ECB WGEM Real and Financial Cycles Team 2016 ====
%

% ---------- UNIVARIATE UNOBSERVED COMPONENTS MODEL FOR CYCLES EXTRACTION ----------
% This Dynare code is a re-implementation of GF1_US.m in BF Cycles code.zip Refer to
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

var		bbeta		$\beta_t$,
		mmu		$\mu_t$,
		ppsi		$\psi_t$,
		ppsist		$\psi^{*}_t$,
		car,
		ym1;

varexo		e_irr		$\epsilon_t$,
		e_lev		$\eta_t$,
		e_slp		$\zeta_t$,
		e_cyc		$\kappa_t$,
		e_cycst		$\kappa^{*}_t$;

parameters	rrho		$\rho$,
		pphi		$\phi$,
		llambda		$\lambda$,
		aa;


%----------------------------------------------------------------
% 2. Calibration
%----------------------------------------------------------------

rrho		= 0.7500;
pphi		= 0.7500;
llambda		= 0.2000;

%----------------------------------------------------------------
% 3. Model
%----------------------------------------------------------------

model(linear);


%%%%% STOCHASTIC TREND AND SLOPE %%%%%

mmu 		=    mmu(-1)  + bbeta(-1) 	+ e_lev;
bbeta		= 		bbeta(-1) 	+ e_slp;


%%%%% CYCLICAL AUTOREGRESSION %%%%%

#rc		= rrho*cos(llambda);
#rs		= rrho*sin(llambda);

ppsi		= rc*ppsi(-1) + rs*ppsist(-1)	+ e_cyc;
ppsist		=-rs*ppsi(-1) + rc*ppsist(-1)	+ e_cycst;
car		=    ppsi(-1) +	pphi*car(-1)	+ 0;

%%%%% MEASUREMENT EQUATION %%%%%

ym1		=    mmu(-1)  +	aa*car(-1)	+ e_irr;

end;

shocks;

var e_cyc; 	stderr 1.0000;
var e_cycst;	stderr 1.0000;

end;


%----------------------------------------------------------------
% 4. Estimated parameters and data
%----------------------------------------------------------------

estimated_params;

%%%%% PARAMETERS %%%%%

	rrho,		beta_pdf,       0.75, 0.20;
	pphi,		beta_pdf,       0.75, 0.20;
	llambda,	normal_pdf,     0.20, 0.20;
	aa,		inv_gamma_pdf,  0.005, inf;

%%%%% INNOVATIONS %%%%%

stderr	e_irr,		inv_gamma_pdf,  0.005, inf;
stderr	e_lev,		inv_gamma_pdf,  0.001, inf;
stderr	e_slp,		inv_gamma_pdf,  0.001, inf;

end;

%
% Observed variables
%

varobs ym1;		% See US_data.m for the correspondence between these
			% observables and US GDP, Credit and Prices series


%----------------------------------------------------------------
% 5. Bayesian estimation and forecasting
%----------------------------------------------------------------

estimation	(
		datafile = US_data,
		nodisplay,
		graph_format = pdf,
		nodiagnostic,
		diffuse_filter,
		kalman_algo = 0,
		mh_replic = 100000,
		mh_nblocks = 1,
		filtered_vars,
		smoother,
		mode_compute = 6,
		plot_priors = 0,
		forecast = 0
		) mmu car;

%----------------------------------------------------------------
% 6. Reporting
%----------------------------------------------------------------

verbatim;

US_data;
stat_		= 'Mean';
eval([ 'trend_ 	= oo_.SmoothedVariables.' stat_ '.mmu'   ';' ]);
eval([ 'cycle_ 	= oo_.SmoothedVariables.' stat_ '.car'   ';' ]);
eval([ 'irreg_ 	= oo_.SmoothedShocks.'    stat_ '.e_irr' ';' ]);

graphs_UOC( Date, ym1, trend_, cycle_, irreg_, 'Series: US log real GDP' );

end;





