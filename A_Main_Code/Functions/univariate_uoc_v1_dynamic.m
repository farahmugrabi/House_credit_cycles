function [residual, g1, g2, g3] = univariate_uoc_v1_dynamic(y, x, params, steady_state, it_)
%
% Status : Computes dynamic model for Dynare
%
% Inputs :
%   y         [#dynamic variables by 1] double    vector of endogenous variables in the order stored
%                                                 in M_.lead_lag_incidence; see the Manual
%   x         [M_.exo_nbr by nperiods] double     matrix of exogenous variables (in declaration order)
%                                                 for all simulation periods
%   params    [M_.param_nbr by 1] double          vector of parameter values in declaration order
%   it_       scalar double                       time period for exogenous variables for which to evaluate the model
%
% Outputs:
%   residual  [M_.endo_nbr by 1] double    vector of residuals of the dynamic model equations in order of 
%                                          declaration of the equations
%   g1        [M_.endo_nbr by #dynamic variables] double    Jacobian matrix of the dynamic model equations;
%                                                           rows: equations in order of declaration
%                                                           columns: variables in order stored in M_.lead_lag_incidence
%   g2        [M_.endo_nbr by (#dynamic variables)^2] double   Hessian matrix of the dynamic model equations;
%                                                              rows: equations in order of declaration
%                                                              columns: variables in order stored in M_.lead_lag_incidence
%   g3        [M_.endo_nbr by (#dynamic variables)^3] double   Third order derivative matrix of the dynamic model equations;
%                                                              rows: equations in order of declaration
%                                                              columns: variables in order stored in M_.lead_lag_incidence
%
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

%
% Model equations
%

residual = zeros(6, 1);
rc__ = params(1)*cos(params(3));
rs__ = params(1)*sin(params(3));
lhs =y(7);
rhs =y(2)+y(1)+x(it_, 2);
residual(1)= lhs-rhs;
lhs =y(6);
rhs =y(1)+x(it_, 3);
residual(2)= lhs-rhs;
lhs =y(8);
rhs =rc__*y(3)+rs__*y(4)+x(it_, 4);
residual(3)= lhs-rhs;
lhs =y(9);
rhs =y(3)*(-rs__)+rc__*y(4)+x(it_, 5);
residual(4)= lhs-rhs;
lhs =y(10);
rhs =y(3)+params(2)*y(5);
residual(5)= lhs-rhs;
lhs =y(11);
rhs =y(2)+y(5)*params(4)+x(it_, 1);
residual(6)= lhs-rhs;
if nargout >= 2,
  g1 = zeros(6, 16);

  %
  % Jacobian matrix
  %

  g1(1,1)=(-1);
  g1(1,2)=(-1);
  g1(1,7)=1;
  g1(1,13)=(-1);
  g1(2,1)=(-1);
  g1(2,6)=1;
  g1(2,14)=(-1);
  g1(3,3)=(-rc__);
  g1(3,8)=1;
  g1(3,4)=(-rs__);
  g1(3,15)=(-1);
  g1(4,3)=rs__;
  g1(4,4)=(-rc__);
  g1(4,9)=1;
  g1(4,16)=(-1);
  g1(5,3)=(-1);
  g1(5,5)=(-params(2));
  g1(5,10)=1;
  g1(6,2)=(-1);
  g1(6,5)=(-params(4));
  g1(6,11)=1;
  g1(6,12)=(-1);
end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],6,256);
end
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],6,4096);
end
end
