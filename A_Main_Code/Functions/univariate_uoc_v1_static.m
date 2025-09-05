function [residual, g1, g2] = univariate_uoc_v1_static(y, x, params)
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

residual = zeros( 6, 1);

%
% Model equations
%

rc__ = params(1)*cos(params(3));
rs__ = params(1)*sin(params(3));
lhs =y(2);
rhs =y(2)+y(1)+x(2);
residual(1)= lhs-rhs;
lhs =y(1);
rhs =y(1)+x(3);
residual(2)= lhs-rhs;
lhs =y(3);
rhs =y(3)*rc__+rs__*y(4)+x(4);
residual(3)= lhs-rhs;
lhs =y(4);
rhs =y(3)*(-rs__)+rc__*y(4)+x(5);
residual(4)= lhs-rhs;
lhs =y(5);
rhs =y(3)+y(5)*params(2);
residual(5)= lhs-rhs;
lhs =y(6);
rhs =y(2)+y(5)*params(4)+x(1);
residual(6)= lhs-rhs;
if ~isreal(residual)
  residual = real(residual)+imag(residual).^2;
end
if nargout >= 2,
  g1 = zeros(6, 6);

  %
  % Jacobian matrix
  %

  g1(1,1)=(-1);
  g1(3,3)=1-rc__;
  g1(3,4)=(-rs__);
  g1(4,3)=rs__;
  g1(4,4)=1-rc__;
  g1(5,3)=(-1);
  g1(5,5)=1-params(2);
  g1(6,2)=(-1);
  g1(6,5)=(-params(4));
  g1(6,6)=1;
  if ~isreal(g1)
    g1 = real(g1)+2*imag(g1);
  end
end
if nargout >= 3,
  %
  % Hessian matrix
  %

  g2 = sparse([],[],[],6,36);
end
end
