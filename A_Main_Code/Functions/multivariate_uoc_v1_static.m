function [residual, g1, g2] = multivariate_uoc_v1_static(y, x, params)
%
% Status : Computes static model for Dynare
%
% Inputs : 
%   y         [M_.endo_nbr by 1] double    vector of endogenous variables in declaration order
%   x         [M_.exo_nbr by 1] double     vector of exogenous variables in declaration order
%   params    [M_.param_nbr by 1] double   vector of parameter values in declaration order
%
% Outputs:
%   residual  [M_.endo_nbr by 1] double    vector of residuals of the static model equations 
%                                          in order of declaration of the equations
%   g1        [M_.endo_nbr by M_.endo_nbr] double    Jacobian matrix of the static model equations;
%                                                     columns: variables in declaration order
%                                                     rows: equations in order of declaration
%   g2        [M_.endo_nbr by (M_.endo_nbr)^2] double   Hessian matrix of the static model equations;
%                                                       columns: variables in declaration order
%                                                       rows: equations in order of declaration
%
%
% Warning : this file is generated automatically by Dynare
%           from model file (.mod)

residual = zeros( 21, 1);

%
% Model equations
%

rc__ = params(1)*cos(params(3));
rs__ = params(1)*sin(params(3));
lhs =y(4);
rhs =y(4)+y(1)+x(4);
residual(1)= lhs-rhs;
lhs =y(1);
rhs =y(1)+x(7);
residual(2)= lhs-rhs;
lhs =y(5);
rhs =y(5)+y(2)+x(5);
residual(3)= lhs-rhs;
lhs =y(2);
rhs =y(2)+x(8);
residual(4)= lhs-rhs;
lhs =y(6);
rhs =y(6)+y(3)+x(6);
residual(5)= lhs-rhs;
lhs =y(3);
rhs =y(3)+x(9);
residual(6)= lhs-rhs;
lhs =y(7);
rhs =y(7)*rc__+rs__*y(10)+x(10);
residual(7)= lhs-rhs;
lhs =y(10);
rhs =y(7)*(-rs__)+rc__*y(10)+x(13);
residual(8)= lhs-rhs;
lhs =y(13);
rhs =y(7)+y(13)*params(2);
residual(9)= lhs-rhs;
lhs =y(16);
rhs =y(10)+params(2)*y(16);
residual(10)= lhs-rhs;
lhs =y(8);
rhs =rc__*y(8)+rs__*y(11)+x(11);
residual(11)= lhs-rhs;
lhs =y(11);
rhs =(-rs__)*y(8)+rc__*y(11)+x(14);
residual(12)= lhs-rhs;
lhs =y(14);
rhs =y(8)+params(2)*y(14);
residual(13)= lhs-rhs;
lhs =y(17);
rhs =y(11)+params(2)*y(17);
residual(14)= lhs-rhs;
lhs =y(9);
rhs =rc__*y(9)+rs__*y(12)+x(12);
residual(15)= lhs-rhs;
lhs =y(12);
rhs =(-rs__)*y(9)+rc__*y(12)+x(15);
residual(16)= lhs-rhs;
lhs =y(15);
rhs =y(9)+params(2)*y(15);
residual(17)= lhs-rhs;
lhs =y(18);
rhs =y(12)+params(2)*y(18);
residual(18)= lhs-rhs;
lhs =y(19);
rhs =y(4)+y(13)*params(4)+x(1);
residual(19)= lhs-rhs;
lhs =y(20);
rhs =y(5)+y(13)*params(5)+y(14)*params(6)+y(16)*params(10)+x(2);
residual(20)= lhs-rhs;
lhs =y(21);
rhs =y(6)+y(13)*params(7)+y(14)*params(8)+y(15)*params(9)+y(16)*params(11)+y(17)*params(12)+x(3);
residual(21)= lhs-rhs;
if ~isreal(residual)
  residual = real(residual)+imag(residual).^2;
end
if nargout >= 2,
  g1 = zeros(21, 21);

  %
  % Jacobian matrix
  %

  g1(1,1)=(-1);
  g1(3,2)=(-1);
  g1(5,3)=(-1);
  g1(7,7)=1-rc__;
  g1(7,10)=(-rs__);
  g1(8,7)=rs__;
  g1(8,10)=1-rc__;
  g1(9,7)=(-1);
  g1(9,13)=1-params(2);
  g1(10,10)=(-1);
  g1(10,16)=1-params(2);
  g1(11,8)=1-rc__;
  g1(11,11)=(-rs__);
  g1(12,8)=rs__;
  g1(12,11)=1-rc__;
  g1(13,8)=(-1);
  g1(13,14)=1-params(2);
  g1(14,11)=(-1);
  g1(14,17)=1-params(2);
  g1(15,9)=1-rc__;
  g1(15,12)=(-rs__);
  g1(16,9)=rs__;
  g1(16,12)=1-rc__;
  g1(17,9)=(-1);
  g1(17,15)=1-params(2);
  g1(18,12)=(-1);
  g1(18,18)=1-params(2);
  g1(19,4)=(-1);
  g1(19,13)=(-params(4));
  g1(19,19)=1;
  g1(20,5)=(-1);
  g1(20,13)=(-params(5));
  g1(20,14)=(-params(6));
  g1(20,16)=(-params(10));
  g1(20,20)=1;
  g1(21,6)=(-1);
  g1(21,13)=(-params(7));
  g1(21,14)=(-params(8));
  g1(21,15)=(-params(9));
  g1(21,16)=(-params(11));
  g1(21,17)=(-params(12));
  g1(21,21)=1;
  if ~isreal(g1)
    g1 = real(g1)+2*imag(g1);
  end
end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],21,441);
end
end
