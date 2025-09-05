function [residual, g1, g2, g3] = multivariate_uoc_v1_dynamic(y, x, params, steady_state, it_)
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

residual = zeros(21, 1);
rc__ = params(1)*cos(params(3));
rs__ = params(1)*sin(params(3));
lhs =y(22);
rhs =y(4)+y(1)+x(it_, 4);
residual(1)= lhs-rhs;
lhs =y(19);
rhs =y(1)+x(it_, 7);
residual(2)= lhs-rhs;
lhs =y(23);
rhs =y(5)+y(2)+x(it_, 5);
residual(3)= lhs-rhs;
lhs =y(20);
rhs =y(2)+x(it_, 8);
residual(4)= lhs-rhs;
lhs =y(24);
rhs =y(6)+y(3)+x(it_, 6);
residual(5)= lhs-rhs;
lhs =y(21);
rhs =y(3)+x(it_, 9);
residual(6)= lhs-rhs;
lhs =y(25);
rhs =rc__*y(7)+rs__*y(10)+x(it_, 10);
residual(7)= lhs-rhs;
lhs =y(28);
rhs =y(7)*(-rs__)+rc__*y(10)+x(it_, 13);
residual(8)= lhs-rhs;
lhs =y(31);
rhs =y(7)+params(2)*y(13);
residual(9)= lhs-rhs;
lhs =y(34);
rhs =y(10)+params(2)*y(16);
residual(10)= lhs-rhs;
lhs =y(26);
rhs =rc__*y(8)+rs__*y(11)+x(it_, 11);
residual(11)= lhs-rhs;
lhs =y(29);
rhs =(-rs__)*y(8)+rc__*y(11)+x(it_, 14);
residual(12)= lhs-rhs;
lhs =y(32);
rhs =y(8)+params(2)*y(14);
residual(13)= lhs-rhs;
lhs =y(35);
rhs =y(11)+params(2)*y(17);
residual(14)= lhs-rhs;
lhs =y(27);
rhs =rc__*y(9)+rs__*y(12)+x(it_, 12);
residual(15)= lhs-rhs;
lhs =y(30);
rhs =(-rs__)*y(9)+rc__*y(12)+x(it_, 15);
residual(16)= lhs-rhs;
lhs =y(33);
rhs =y(9)+params(2)*y(15);
residual(17)= lhs-rhs;
lhs =y(36);
rhs =y(12)+params(2)*y(18);
residual(18)= lhs-rhs;
lhs =y(37);
rhs =y(4)+y(13)*params(4)+x(it_, 1);
residual(19)= lhs-rhs;
lhs =y(38);
rhs =y(5)+y(13)*params(5)+y(14)*params(6)+y(16)*params(10)+x(it_, 2);
residual(20)= lhs-rhs;
lhs =y(39);
rhs =y(6)+y(13)*params(7)+y(14)*params(8)+y(15)*params(9)+y(16)*params(11)+y(17)*params(12)+x(it_, 3);
residual(21)= lhs-rhs;
if nargout >= 2,
  g1 = zeros(21, 54);

  %
  % Jacobian matrix
  %

  g1(1,1)=(-1);
  g1(1,4)=(-1);
  g1(1,22)=1;
  g1(1,43)=(-1);
  g1(2,1)=(-1);
  g1(2,19)=1;
  g1(2,46)=(-1);
  g1(3,2)=(-1);
  g1(3,5)=(-1);
  g1(3,23)=1;
  g1(3,44)=(-1);
  g1(4,2)=(-1);
  g1(4,20)=1;
  g1(4,47)=(-1);
  g1(5,3)=(-1);
  g1(5,6)=(-1);
  g1(5,24)=1;
  g1(5,45)=(-1);
  g1(6,3)=(-1);
  g1(6,21)=1;
  g1(6,48)=(-1);
  g1(7,7)=(-rc__);
  g1(7,25)=1;
  g1(7,10)=(-rs__);
  g1(7,49)=(-1);
  g1(8,7)=rs__;
  g1(8,10)=(-rc__);
  g1(8,28)=1;
  g1(8,52)=(-1);
  g1(9,7)=(-1);
  g1(9,13)=(-params(2));
  g1(9,31)=1;
  g1(10,10)=(-1);
  g1(10,16)=(-params(2));
  g1(10,34)=1;
  g1(11,8)=(-rc__);
  g1(11,26)=1;
  g1(11,11)=(-rs__);
  g1(11,50)=(-1);
  g1(12,8)=rs__;
  g1(12,11)=(-rc__);
  g1(12,29)=1;
  g1(12,53)=(-1);
  g1(13,8)=(-1);
  g1(13,14)=(-params(2));
  g1(13,32)=1;
  g1(14,11)=(-1);
  g1(14,17)=(-params(2));
  g1(14,35)=1;
  g1(15,9)=(-rc__);
  g1(15,27)=1;
  g1(15,12)=(-rs__);
  g1(15,51)=(-1);
  g1(16,9)=rs__;
  g1(16,12)=(-rc__);
  g1(16,30)=1;
  g1(16,54)=(-1);
  g1(17,9)=(-1);
  g1(17,15)=(-params(2));
  g1(17,33)=1;
  g1(18,12)=(-1);
  g1(18,18)=(-params(2));
  g1(18,36)=1;
  g1(19,4)=(-1);
  g1(19,13)=(-params(4));
  g1(19,37)=1;
  g1(19,40)=(-1);
  g1(20,5)=(-1);
  g1(20,13)=(-params(5));
  g1(20,14)=(-params(6));
  g1(20,16)=(-params(10));
  g1(20,38)=1;
  g1(20,41)=(-1);
  g1(21,6)=(-1);
  g1(21,13)=(-params(7));
  g1(21,14)=(-params(8));
  g1(21,15)=(-params(9));
  g1(21,16)=(-params(11));
  g1(21,17)=(-params(12));
  g1(21,39)=1;
  g1(21,42)=(-1);
end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],21,2916);
end
if nargout >= 4,
  %
  % Third order derivatives
  %

  g3 = sparse([],[],[],21,157464);
end
end
