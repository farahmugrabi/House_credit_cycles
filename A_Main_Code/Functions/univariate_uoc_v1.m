%
% Status : main Dynare file 
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

clear all
tic;
global M_ oo_ options_ ys0_ ex0_ estimation_info
options_ = [];
M_.fname = 'univariate_uoc_v1';
%
% Some global variables initialization
%
global_initialization;
diary off;
diary('univariate_uoc_v1.log');
M_.exo_names = 'e_irr';
M_.exo_names_tex = '\epsilon_t';
M_.exo_names_long = 'e_irr';
M_.exo_names = char(M_.exo_names, 'e_lev');
M_.exo_names_tex = char(M_.exo_names_tex, '\eta_t');
M_.exo_names_long = char(M_.exo_names_long, 'e_lev');
M_.exo_names = char(M_.exo_names, 'e_slp');
M_.exo_names_tex = char(M_.exo_names_tex, '\zeta_t');
M_.exo_names_long = char(M_.exo_names_long, 'e_slp');
M_.exo_names = char(M_.exo_names, 'e_cyc');
M_.exo_names_tex = char(M_.exo_names_tex, '\kappa_t');
M_.exo_names_long = char(M_.exo_names_long, 'e_cyc');
M_.exo_names = char(M_.exo_names, 'e_cycst');
M_.exo_names_tex = char(M_.exo_names_tex, '\kappa^{*}_t');
M_.exo_names_long = char(M_.exo_names_long, 'e_cycst');
M_.endo_names = 'bbeta';
M_.endo_names_tex = '\beta_t';
M_.endo_names_long = 'bbeta';
M_.endo_names = char(M_.endo_names, 'mmu');
M_.endo_names_tex = char(M_.endo_names_tex, '\mu_t');
M_.endo_names_long = char(M_.endo_names_long, 'mmu');
M_.endo_names = char(M_.endo_names, 'ppsi');
M_.endo_names_tex = char(M_.endo_names_tex, '\psi_t');
M_.endo_names_long = char(M_.endo_names_long, 'ppsi');
M_.endo_names = char(M_.endo_names, 'ppsist');
M_.endo_names_tex = char(M_.endo_names_tex, '\psi^{*}_t');
M_.endo_names_long = char(M_.endo_names_long, 'ppsist');
M_.endo_names = char(M_.endo_names, 'car');
M_.endo_names_tex = char(M_.endo_names_tex, 'car');
M_.endo_names_long = char(M_.endo_names_long, 'car');
M_.endo_names = char(M_.endo_names, 'ym1');
M_.endo_names_tex = char(M_.endo_names_tex, 'ym1');
M_.endo_names_long = char(M_.endo_names_long, 'ym1');
M_.param_names = 'rrho';
M_.param_names_tex = '\rho';
M_.param_names_long = 'rrho';
M_.param_names = char(M_.param_names, 'pphi');
M_.param_names_tex = char(M_.param_names_tex, '\phi');
M_.param_names_long = char(M_.param_names_long, 'pphi');
M_.param_names = char(M_.param_names, 'llambda');
M_.param_names_tex = char(M_.param_names_tex, '\lambda');
M_.param_names_long = char(M_.param_names_long, 'llambda');
M_.param_names = char(M_.param_names, 'aa');
M_.param_names_tex = char(M_.param_names_tex, 'aa');
M_.param_names_long = char(M_.param_names_long, 'aa');
M_.exo_det_nbr = 0;
M_.exo_nbr = 5;
M_.endo_nbr = 6;
M_.param_nbr = 4;
M_.orig_endo_nbr = 6;
M_.aux_vars = [];
options_.varobs = [];
options_.varobs = 'ym1';
options_.varobs_id = [ 6  ];
M_.Sigma_e = zeros(5, 5);
M_.Correlation_matrix = eye(5, 5);
M_.H = 0;
M_.Correlation_matrix_ME = 1;
M_.sigma_e_is_diagonal = 1;
options_.linear = 1;
options_.block=0;
options_.bytecode=0;
options_.use_dll=0;
erase_compiled_function('univariate_uoc_v1_static');
erase_compiled_function('univariate_uoc_v1_dynamic');
M_.lead_lag_incidence = [
 1 6;
 2 7;
 3 8;
 4 9;
 5 10;
 0 11;]';
M_.nstatic = 1;
M_.nfwrd   = 0;
M_.npred   = 5;
M_.nboth   = 0;
M_.nsfwrd   = 0;
M_.nspred   = 5;
M_.ndynamic   = 5;
M_.equations_tags = {
};
M_.static_and_dynamic_models_differ = 0;
M_.exo_names_orig_ord = [1:5];
M_.maximum_lag = 1;
M_.maximum_lead = 0;
M_.maximum_endo_lag = 1;
M_.maximum_endo_lead = 0;
oo_.steady_state = zeros(6, 1);
M_.maximum_exo_lag = 0;
M_.maximum_exo_lead = 0;
oo_.exo_steady_state = zeros(5, 1);
M_.params = NaN(4, 1);
M_.NNZDerivatives = zeros(3, 1);
M_.NNZDerivatives(1) = 22;
M_.NNZDerivatives(2) = 0;
M_.NNZDerivatives(3) = -1;
M_.params( 1 ) = 0.7500;
rrho = M_.params( 1 );
M_.params( 2 ) = 0.7500;
pphi = M_.params( 2 );
M_.params( 3 ) = 0.2000;
llambda = M_.params( 3 );
%
% SHOCKS instructions
%
make_ex_;
M_.exo_det_length = 0;
M_.Sigma_e(4, 4) = (1.0000)^2;
M_.Sigma_e(5, 5) = (1.0000)^2;
global estim_params_
estim_params_.var_exo = [];
estim_params_.var_endo = [];
estim_params_.corrx = [];
estim_params_.corrn = [];
estim_params_.param_vals = [];
estim_params_.param_vals = [estim_params_.param_vals; 1, NaN, (-Inf), Inf, 1, 0.75, 0.20, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 2, NaN, (-Inf), Inf, 1, 0.75, 0.20, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 3, NaN, (-Inf), Inf, 3, 0.20, 0.20, NaN, NaN, NaN ];
estim_params_.param_vals = [estim_params_.param_vals; 4, NaN, (-Inf), Inf, 4, 0.005, Inf, NaN, NaN, NaN ];
estim_params_.var_exo = [estim_params_.var_exo; 1, NaN, (-Inf), Inf, 4, 0.005, Inf, NaN, NaN, NaN ];
estim_params_.var_exo = [estim_params_.var_exo; 2, NaN, (-Inf), Inf, 4, 0.001, Inf, NaN, NaN, NaN ];
estim_params_.var_exo = [estim_params_.var_exo; 3, NaN, (-Inf), Inf, 4, 0.001, Inf, NaN, NaN, NaN ];
options_.diffuse_filter = 1;
options_.filtered_vars = 1;
options_.forecast = 0;
options_.kalman_algo = 0;
options_.mh_nblck = 1;
options_.mh_replic = 100000;
options_.mode_compute = 6;
options_.nodiagnostic = 1;
options_.nodisplay = 1;
options_.plot_priors = 0;
options_.smoother = 1;
options_.datafile = 'US_data';
options_.graph_format=[];
options_.graph_format = 'pdf';
options_.order = 1;
options_.steadystate.nocheck = 1;
var_list_=[];
var_list_ = 'mmu';
var_list_ = char(var_list_, 'car');
dynare_estimation(var_list_);
US_data;
stat_		= 'Mean';
eval([ 'trend_ 	= oo_.SmoothedVariables.' stat_ '.mmu'   ';' ]);
eval([ 'cycle_ 	= oo_.SmoothedVariables.' stat_ '.car'   ';' ]);
eval([ 'irreg_ 	= oo_.SmoothedShocks.'    stat_ '.e_irr' ';' ]);
graphs_UOC( Date, ym1, trend_, cycle_, irreg_, 'Series: US log real GDP' );
save('univariate_uoc_v1_results.mat', 'oo_', 'M_', 'options_');
if exist('estim_params_', 'var') == 1
  save('univariate_uoc_v1_results.mat', 'estim_params_', '-append');
end
if exist('bayestopt_', 'var') == 1
  save('univariate_uoc_v1_results.mat', 'bayestopt_', '-append');
end
if exist('dataset_', 'var') == 1
  save('univariate_uoc_v1_results.mat', 'dataset_', '-append');
end
if exist('estimation_info', 'var') == 1
  save('univariate_uoc_v1_results.mat', 'estimation_info', '-append');
end

disp(['Total computing time : ' dynsec2hms(toc) ]);
if ~isempty(lastwarn)
  disp('Note: warning(s) encountered in MATLAB/Octave code')
end
diary off
